module FFmpeg.Audio.MP4
    ( SpatialMp4Config(..)
    , defaultSpatialMp4Config
    , encodeSpatialMp4
    , encodeSpatialMp4BS
    , EclipsaMp4Config(..)
    , defaultEclipsaMp4Config
    , encodeEclipsaMp4
    , encodeEclipsaMp4BS
    , SurroundM4aConfig(..)
    , defaultSurroundM4aConfig
    , encodeSurroundM4a
    , encodeSurroundM4aBS
    ) where

import Control.Exception (SomeException, bracket_, catch, finally, throwIO)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.List (sortOn)
import Foreign.Storable (pokeByteOff)
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)

import FFmpeg.Audio.IAMF
    ( IamfConfig(..)
    , IamfContainer(..)
    , IamfElement(..)
    , IamfSubstreamCodec(..)
    , defaultIamfConfig
    )
import FFmpeg.Audio.Internal.Codec
    ( codecSupportsSmallLastFrame
    , findEncoderByName
    )
import FFmpeg.Audio.Internal.CodecContext
    ( CodecContextHandle
    , CodecContextParams(..)
    , VideoCodecContextParams(..)
    , getCodecFrameSize
    , getCodecTimeBase
    , withCodecContextWith
    , withVideoCodecContext
    )
import FFmpeg.Audio.Internal.Encoder
    ( AudioCodec(..)
    , CodecConfig(..)
    , applyFormatMetadata
    , codecNameFor
    , defaultAacConfig
    , encodeAllFrames
    , encodeAudioToFileWithMetadata
    , flushEncoderStreamWith
    , sendFrameAndDrainWith
    )
import FFmpeg.Audio.Internal.Packet (withPacket)
import FFmpeg.Audio.Internal.Error (FFmpegError(..))
import FFmpeg.Audio.Internal.Format
    ( FormatContextHandle
    , StreamHandle
    , closeOutput
    , configureMovTextStream
    , createRawStream
    , createStream
    , formatNeedsGlobalHeader
    , openOutput
    , setCodecParameters
    , setFormatStrictStdCompliance
    , setStreamAvgFrameRate
    , setStreamDisposition
    , setStreamMetadata
    , setStreamTimeBase
    , withFormatContextWith
    , writeHeaderWithOptions
    , writeMovTextPacket
    , writeTrailer
    )
import FFmpeg.Audio.Internal.Frame
    ( getFramePtr
    , makeFrameWritable
    , withVideoFrame
    )
import FFmpeg.Audio.Internal.IamfMux
    ( encodeAllSubstreams
    , iamfTotalFrames
    , withIamfPrepared
    )
import FFmpeg.Audio.Internal.Image
    ( Yuv420Image(..)
    , decodeCoverToYuv420
    , defaultDarkSlateImage
    , fillVideoFrame
    )
import FFmpeg.Audio.Internal.Types
    ( AVRational(..)
    , avDispositionDefault
    , avPixFmtYuv420p
    , avSampleFormatFLTP
    , framePtsOffset
    )
import FFmpeg.Audio.Metadata
    ( AudioMetadata(..)
    , TimedLyric(..)
    , emptyAudioMetadata
    , encodeUtf8
    )
import FFmpeg.Audio.PCMBuffer
    ( AudioBuffer(..)
    , ChannelLayout(..)
    , audioChannels
    , audioFrameCount
    )

data SpatialMp4Config = SpatialMp4Config
    { smp4CoverImage      :: !(Maybe BS.ByteString)
    , smp4Metadata        :: !AudioMetadata
    , smp4BinauralBitRate :: !Int
    , smp4SurroundBitRate :: !Int
    , smp4VideoFps        :: !Int
    } deriving (Show, Eq)

defaultSpatialMp4Config :: SpatialMp4Config
defaultSpatialMp4Config = SpatialMp4Config
    { smp4CoverImage = Nothing
    , smp4Metadata = emptyAudioMetadata
    , smp4BinauralBitRate = 384000
    , smp4SurroundBitRate = 512000
    , smp4VideoFps = 1
    }

data EclipsaMp4Config = EclipsaMp4Config
    { emp4CoverImage :: !(Maybe BS.ByteString)
    , emp4Metadata   :: !AudioMetadata
    , emp4IamfConfig :: !IamfConfig
    , emp4VideoFps   :: !Int
    } deriving (Show, Eq)

defaultEclipsaMp4Config :: EclipsaMp4Config
defaultEclipsaMp4Config = EclipsaMp4Config
    { emp4CoverImage = Nothing
    , emp4Metadata = emptyAudioMetadata
    , emp4IamfConfig = defaultIamfConfig
        { iamfContainer = IamfMp4
        , iamfCodec = IamfCodecOpus
        , iamfSampleRate = 48000
        , iamfBitRatePerSubstream = Just 128000
        , iamfTargetLayouts = [LayoutStereo, LayoutBinaural]
        }
    , emp4VideoFps = 1
    }

data SurroundM4aConfig = SurroundM4aConfig
    { sm4aMetadata :: !AudioMetadata
    , sm4aBitRate  :: !Int
    } deriving (Show, Eq)

defaultSurroundM4aConfig :: SurroundM4aConfig
defaultSurroundM4aConfig = SurroundM4aConfig
    { sm4aMetadata = emptyAudioMetadata
    , sm4aBitRate = 512000
    }

resolveCoverImage :: Maybe BS.ByteString -> IO Yuv420Image
resolveCoverImage Nothing = return defaultDarkSlateImage
resolveCoverImage (Just bs)
    | BS.null bs = return defaultDarkSlateImage
    | otherwise = decodeCoverToYuv420 bs

videoCodecParams :: Yuv420Image -> Int -> Bool -> VideoCodecContextParams
videoCodecParams img fps needsGlobal =
    VideoCodecContextParams
        { vcWidth = yuvWidth img
        , vcHeight = yuvHeight img
        , vcPixFmt = avPixFmtYuv420p
        , vcTimeBase = AVRational 1 (fromIntegral fps)
        , vcFramerate = AVRational (fromIntegral fps) 1
        , vcGopSize = max 2 (fps * 4)
        , vcMaxBFrames = 0
        , vcGlobalHeader = needsGlobal
        , vcStringOptions =
            [ ("preset", "ultrafast")
            , ("tune", "stillimage")
            , ("crf", "23")
            , ("profile", "high")
            ]
        }

encodeStaticVideo
    :: FormatContextHandle
    -> CodecContextHandle
    -> StreamHandle
    -> AVRational
    -> Yuv420Image
    -> Int
    -> IO ()
encodeStaticVideo fmt vCtx vStream vTb img totalFrames =
    withPacket $ \ph -> do
        withVideoFrame (yuvWidth img) (yuvHeight img) avPixFmtYuv420p $ \fh -> do
            makeFrameWritable fh
            fillVideoFrame fh img
            let loop !idx
                    | idx >= totalFrames = return ()
                    | otherwise = do
                        makeFrameWritable fh
                        pokeByteOff (getFramePtr fh) framePtsOffset (fromIntegral idx :: Int64)
                        sendFrameAndDrainWith vCtx fmt vStream vTb fh ph
                        loop (idx + 1)
            loop 0
        flushEncoderStreamWith vCtx fmt vStream vTb ph

normalizeTimedLyrics :: [TimedLyric] -> [TimedLyric]
normalizeTimedLyrics raw =
    let cleaned = sortOn tlTimestampMs [TimedLyric (max 0 ts) txt | TimedLyric ts txt <- raw, not (null txt)]
        go _ [] = []
        go minTs (TimedLyric ts txt : rest) =
            let ts' = max minTs ts
            in TimedLyric ts' txt : go (ts' + 1) rest
    in go 0 cleaned

writeTimedLyrics :: FormatContextHandle -> StreamHandle -> Int64 -> [TimedLyric] -> IO ()
writeTimedLyrics _ _ _ [] = return ()
writeTimedLyrics fmt subStream totalDurMs lyrics = do
    case lyrics of
        (TimedLyric firstTs _ : _) | firstTs > 0 ->
            writeMovTextPacket fmt subStream 0 (fromIntegral firstTs) BS.empty
        _ -> return ()
    let go [] = return ()
        go [TimedLyric ts txt] = do
            let startMs = fromIntegral ts :: Int64
                endMs = max (startMs + 1000) totalDurMs
                durMs = max 1 (endMs - startMs)
            writeMovTextPacket fmt subStream startMs durMs (encodeUtf8 txt)
        go (TimedLyric ts txt : next@(TimedLyric nextTs _) : rest) = do
            let startMs = fromIntegral ts :: Int64
                durMs = max 1 (fromIntegral nextTs - startMs)
            writeMovTextPacket fmt subStream startMs durMs (encodeUtf8 txt)
            go (next : rest)
    go lyrics

encodeSpatialMp4 :: FilePath -> SpatialMp4Config -> AudioBuffer -> AudioBuffer -> IO ()
encodeSpatialMp4 path cfg binBuf surBuf = do
    when (audioChannels binBuf /= 2) $
        throwIO $ FFmpegError (-1)
            ("encodeSpatialMp4: binaural buffer must have 2 channels, got " ++ show (audioChannels binBuf))
    when (audioChannels surBuf /= 6) $
        throwIO $ FFmpegError (-1)
            ("encodeSpatialMp4: surround buffer must have 6 channels (5.1), got " ++ show (audioChannels surBuf))
    when (audioSampleRate binBuf <= 0 || audioSampleRate surBuf <= 0) $
        throwIO $ FFmpegError (-1) "encodeSpatialMp4: invalid sample rate"
    img <- resolveCoverImage (smp4CoverImage cfg)
    let fps = max 1 (smp4VideoFps cfg)
        durBin = fromIntegral (audioFrameCount binBuf) / fromIntegral (audioSampleRate binBuf) :: Double
        durSur = fromIntegral (audioFrameCount surBuf) / fromIntegral (audioSampleRate surBuf) :: Double
        durSec = max 0.5 (max durBin durSur)
        durMs = max 1 (round (durSec * 1000.0) :: Int64)
        totalVideoFrames = max 1 (ceiling (durSec * fromIntegral fps) :: Int)
        lyrics = normalizeTimedLyrics (metaSyncedLyrics (smp4Metadata cfg))
    vCodec <- findEncoderByName "libx264"
    aCodec <- findEncoderByName (codecNameFor CodecAac)
    allowShort <- codecSupportsSmallLastFrame aCodec
    withFormatContextWith (Just "mp4") path $ \fmt -> do
        setFormatStrictStdCompliance fmt (-2)
        applyFormatMetadata fmt (smp4Metadata cfg)
        needsGlobal <- formatNeedsGlobalHeader fmt
        let vParams = videoCodecParams img fps needsGlobal
            binParams = CodecContextParams
                { ccSampleRate = audioSampleRate binBuf
                , ccChannelLayout = LayoutStereo
                , ccSampleFormat = avSampleFormatFLTP
                , ccBitRate = Just (smp4BinauralBitRate cfg)
                , ccBitsPerRawSample = Nothing
                , ccGlobalHeader = needsGlobal
                , ccIntOptions = []
                }
            surParams = CodecContextParams
                { ccSampleRate = audioSampleRate surBuf
                , ccChannelLayout = Layout5_1
                , ccSampleFormat = avSampleFormatFLTP
                , ccBitRate = Just (smp4SurroundBitRate cfg)
                , ccBitsPerRawSample = Nothing
                , ccGlobalHeader = needsGlobal
                , ccIntOptions = []
                }
        withVideoCodecContext vCodec vParams $ \vCtx ->
            withCodecContextWith aCodec binParams $ \binCtx ->
                withCodecContextWith aCodec surParams $ \surCtx -> do
                    vStream <- createStream fmt vCodec
                    vTb <- getCodecTimeBase vCtx
                    setStreamTimeBase vStream vTb
                    setStreamAvgFrameRate vStream (AVRational (fromIntegral fps) 1)
                    setCodecParameters vStream vCtx
                    binStream <- createStream fmt aCodec
                    binTb <- getCodecTimeBase binCtx
                    setStreamTimeBase binStream binTb
                    setCodecParameters binStream binCtx
                    setStreamDisposition binStream avDispositionDefault
                    setStreamMetadata binStream "language" "eng"
                    setStreamMetadata binStream "title" "Binaural 3D (Headphones)"
                    setStreamMetadata binStream "handler_name" "Binaural 3D (Headphones)"
                    surStream <- createStream fmt aCodec
                    surTb <- getCodecTimeBase surCtx
                    setStreamTimeBase surStream surTb
                    setCodecParameters surStream surCtx
                    setStreamDisposition surStream 0
                    setStreamMetadata surStream "language" "eng"
                    setStreamMetadata surStream "title" "5.1 Surround"
                    setStreamMetadata surStream "handler_name" "5.1 Surround"
                    mSubStream <-
                        if null lyrics
                            then return Nothing
                            else do
                                sh <- createRawStream fmt
                                configureMovTextStream sh
                                setStreamMetadata sh "language" "eng"
                                setStreamMetadata sh "title" "Lyrics"
                                setStreamMetadata sh "handler_name" "Lyrics"
                                return (Just sh)
                    binFsRaw <- getCodecFrameSize binCtx
                    surFsRaw <- getCodecFrameSize surCtx
                    let binFs = if binFsRaw > 0 then binFsRaw else 1024
                        surFs = if surFsRaw > 0 then surFsRaw else 1024
                    bracket_
                        (openOutput fmt path)
                        (closeOutput fmt `catch` (\(_ :: SomeException) -> return ()))
                        (do
                            writeHeaderWithOptions fmt [("movflags", "+faststart")]
                            encodeStaticVideo fmt vCtx vStream vTb img totalVideoFrames
                            encodeAllFrames binCtx fmt binStream binTb
                                (audioSamples binBuf) LayoutStereo 2 (audioSampleRate binBuf)
                                avSampleFormatFLTP binFs allowShort (audioFrameCount binBuf)
                            encodeAllFrames surCtx fmt surStream surTb
                                (audioSamples surBuf) Layout5_1 6 (audioSampleRate surBuf)
                                avSampleFormatFLTP surFs allowShort (audioFrameCount surBuf)
                            case mSubStream of
                                Just subStream -> writeTimedLyrics fmt subStream durMs lyrics
                                Nothing -> return ()
                            writeTrailer fmt)

encodeSpatialMp4BS :: SpatialMp4Config -> AudioBuffer -> AudioBuffer -> IO BS.ByteString
encodeSpatialMp4BS cfg binBuf surBuf = do
    (path, h) <- openTempFile "/tmp" "ffmpeg-audio-spatial.mp4"
    hClose h
    finally
        (encodeSpatialMp4 path cfg binBuf surBuf >> BS.readFile path)
        (removeFile path `catch` (\(_ :: SomeException) -> return ()))

encodeEclipsaMp4 :: FilePath -> EclipsaMp4Config -> [IamfElement] -> IO ()
encodeEclipsaMp4 path cfg elements = do
    img <- resolveCoverImage (emp4CoverImage cfg)
    let fps = max 1 (emp4VideoFps cfg)
        iamfCfg = (emp4IamfConfig cfg) { iamfContainer = IamfMp4 }
        sr = iamfSampleRate iamfCfg
    vCodec <- findEncoderByName "libx264"
    withFormatContextWith (Just "mp4") path $ \fmt -> do
        setFormatStrictStdCompliance fmt (-2)
        applyFormatMetadata fmt (emp4Metadata cfg)
        needsGlobal <- formatNeedsGlobalHeader fmt
        withIamfPrepared fmt iamfCfg elements $ \(opened, frameSize, allowShort) -> do
            let durSec = max 0.5 (fromIntegral (iamfTotalFrames opened) / fromIntegral sr :: Double)
                totalVideoFrames = max 1 (ceiling (durSec * fromIntegral fps) :: Int)
                vParams = videoCodecParams img fps needsGlobal
            withVideoCodecContext vCodec vParams $ \vCtx -> do
                vStream <- createStream fmt vCodec
                vTb <- getCodecTimeBase vCtx
                setStreamTimeBase vStream vTb
                setStreamAvgFrameRate vStream (AVRational (fromIntegral fps) 1)
                setCodecParameters vStream vCtx
                bracket_
                    (openOutput fmt path)
                    (closeOutput fmt `catch` (\(_ :: SomeException) -> return ()))
                    (do
                        writeHeaderWithOptions fmt [("movflags", "+faststart")]
                        encodeAllSubstreams fmt opened sr frameSize allowShort
                        encodeStaticVideo fmt vCtx vStream vTb img totalVideoFrames
                        writeTrailer fmt)

encodeEclipsaMp4BS :: EclipsaMp4Config -> [IamfElement] -> IO BS.ByteString
encodeEclipsaMp4BS cfg elements = do
    (path, h) <- openTempFile "/tmp" "ffmpeg-audio-eclipsa.mp4"
    hClose h
    finally
        (encodeEclipsaMp4 path cfg elements >> BS.readFile path)
        (removeFile path `catch` (\(_ :: SomeException) -> return ()))

encodeSurroundM4a :: FilePath -> SurroundM4aConfig -> AudioBuffer -> IO ()
encodeSurroundM4a path cfg buf = do
    when (audioChannels buf /= 6) $
        throwIO $ FFmpegError (-1)
            ("encodeSurroundM4a: surround buffer must have 6 channels (5.1), got " ++ show (audioChannels buf))
    let aacCfg = defaultAacConfig
            { codecBitRate = Just (sm4aBitRate cfg)
            , codecFormatName = Just "ipod"
            }
        surBuf = buf { audioLayout = Layout5_1 }
    encodeAudioToFileWithMetadata path aacCfg (sm4aMetadata cfg) surBuf

encodeSurroundM4aBS :: SurroundM4aConfig -> AudioBuffer -> IO BS.ByteString
encodeSurroundM4aBS cfg buf = do
    (path, h) <- openTempFile "/tmp" "ffmpeg-audio-surround.m4a"
    hClose h
    finally
        (encodeSurroundM4a path cfg buf >> BS.readFile path)
        (removeFile path `catch` (\(_ :: SomeException) -> return ()))
