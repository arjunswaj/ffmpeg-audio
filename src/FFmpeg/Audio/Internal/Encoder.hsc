module FFmpeg.Audio.Internal.Encoder
    ( encodePcmToFile
    , AudioCodec(..)
    , CodecConfig(..)
    , defaultMp3Config
    , defaultFlac24Config
    , defaultOpusConfig
    , defaultOpusAmbisonicsConfig
    , defaultAacConfig
    , defaultEac3Config
    , encodeAudioToFile
    , codecNameFor
    , defaultExtensionFor
    , selectSampleFormat
    , fillAudioFrame
    , sendFrameAndDrain
    , flushEncoderStream
    ) where

import Control.Exception (SomeException, bracket_, catch, throwIO)
import Control.Monad (when)
import Data.Bits (shiftL, shiftR)
import Data.Char (toLower)
import Data.Int (Int16, Int32, Int64)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Foreign.C.Types (CUChar)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable, peekByteOff, peekElemOff, pokeByteOff, pokeElemOff, sizeOf)
import FFmpeg.Audio.Internal.Codec
    ( codecSupportsSmallLastFrame
    , findEncoderByName
    , findMp3Encoder
    )
import FFmpeg.Audio.Internal.CodecContext
    ( CodecContextHandle
    , CodecContextParams(..)
    , getCodecContextPtr
    , getCodecFrameSize
    , getCodecTimeBase
    , withCodecContext
    , withCodecContextWith
    )
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Format
    ( FormatContextHandle
    , StreamHandle
    , closeOutput
    , createStream
    , formatNeedsGlobalHeader
    , openOutput
    , rescaleAndWriteFrame
    , setCodecParameters
    , setFormatStrictStdCompliance
    , setStreamTimeBase
    , withFormatContextWith
    , withOutputFile
    , writeFrame
    , writeHeader
    , writeTrailer
    )
import FFmpeg.Audio.Internal.Frame
    ( FrameHandle
    , getFramePtr
    , makeFrameWritable
    , withFrame
    , withFrameWithLayout
    )
import FFmpeg.Audio.Internal.Packet (getPacketPtr, unrefPacket, withPacket)
import FFmpeg.Audio.Internal.Types
    ( AVFrame
    , AVRational
    , AVSampleFormat
    , avSampleFormatFLT
    , avSampleFormatFLTP
    , avSampleFormatS16
    , avSampleFormatS16P
    , avSampleFormatS32
    , avSampleFormatS32P
    , averrorEagain
    , averrorEof
    , frameDataArrayOffset
    , frameExtendedDataOffset
    , framePtsOffset
    )
import FFmpeg.Audio.PCMBuffer
    ( AudioBuffer(..)
    , ChannelLayout(..)
    , PCMBuffer(..)
    , SampleData(..)
    , audioChannels
    , audioFrameCount
    )

data AudioCodec
    = CodecMp3
    | CodecFlac
    | CodecOpus
    | CodecAac
    | CodecEac3
    | CodecPcmS16Le
    | CodecPcmS24Le
    deriving (Show, Eq)

data CodecConfig = CodecConfig
    { codecType              :: !AudioCodec
    , codecBitRate           :: !(Maybe Int)
    , codecBitDepth          :: !(Maybe Int)
    , codecFormatName        :: !(Maybe String)
    , codecOpusMappingFamily :: !(Maybe Int)
    } deriving (Show, Eq)

defaultMp3Config :: CodecConfig
defaultMp3Config = CodecConfig
    { codecType = CodecMp3
    , codecBitRate = Just 320000
    , codecBitDepth = Nothing
    , codecFormatName = Nothing
    , codecOpusMappingFamily = Nothing
    }

defaultFlac24Config :: CodecConfig
defaultFlac24Config = CodecConfig
    { codecType = CodecFlac
    , codecBitRate = Nothing
    , codecBitDepth = Just 24
    , codecFormatName = Nothing
    , codecOpusMappingFamily = Nothing
    }

defaultOpusConfig :: CodecConfig
defaultOpusConfig = CodecConfig
    { codecType = CodecOpus
    , codecBitRate = Just 256000
    , codecBitDepth = Nothing
    , codecFormatName = Nothing
    , codecOpusMappingFamily = Nothing
    }

defaultOpusAmbisonicsConfig :: Int -> CodecConfig
defaultOpusAmbisonicsConfig family = CodecConfig
    { codecType = CodecOpus
    , codecBitRate = Just 256000
    , codecBitDepth = Nothing
    , codecFormatName = Just "opus"
    , codecOpusMappingFamily = Just family
    }

defaultAacConfig :: CodecConfig
defaultAacConfig = CodecConfig
    { codecType = CodecAac
    , codecBitRate = Just 256000
    , codecBitDepth = Nothing
    , codecFormatName = Nothing
    , codecOpusMappingFamily = Nothing
    }

defaultEac3Config :: CodecConfig
defaultEac3Config = CodecConfig
    { codecType = CodecEac3
    , codecBitRate = Just 448000
    , codecBitDepth = Nothing
    , codecFormatName = Nothing
    , codecOpusMappingFamily = Nothing
    }

codecNameFor :: AudioCodec -> String
codecNameFor CodecMp3 = "libmp3lame"
codecNameFor CodecFlac = "flac"
codecNameFor CodecOpus = "libopus"
codecNameFor CodecAac = "aac"
codecNameFor CodecEac3 = "eac3"
codecNameFor CodecPcmS16Le = "pcm_s16le"
codecNameFor CodecPcmS24Le = "pcm_s24le"

defaultMuxerFor :: AudioCodec -> Maybe String
defaultMuxerFor CodecMp3 = Just "mp3"
defaultMuxerFor CodecFlac = Just "flac"
defaultMuxerFor CodecOpus = Just "opus"
defaultMuxerFor CodecAac = Just "adts"
defaultMuxerFor CodecEac3 = Just "eac3"
defaultMuxerFor CodecPcmS16Le = Just "wav"
defaultMuxerFor CodecPcmS24Le = Just "wav"

defaultExtensionFor :: AudioCodec -> String
defaultExtensionFor CodecMp3 = ".mp3"
defaultExtensionFor CodecFlac = ".flac"
defaultExtensionFor CodecOpus = ".opus"
defaultExtensionFor CodecAac = ".aac"
defaultExtensionFor CodecEac3 = ".eac3"
defaultExtensionFor CodecPcmS16Le = ".wav"
defaultExtensionFor CodecPcmS24Le = ".wav"

hasKnownContainerExt :: FilePath -> Bool
hasKnownContainerExt fp =
    let lower = map toLower fp
    in any (`isSuffixOf` lower)
        [ ".mp3", ".flac", ".opus", ".ogg", ".mp4", ".m4a"
        , ".aac", ".eac3", ".wav", ".mkv", ".iamf"
        ]

resolveMuxerName :: FilePath -> CodecConfig -> Maybe String
resolveMuxerName path cfg = case codecFormatName cfg of
    Just f -> Just f
    Nothing ->
        if hasKnownContainerExt path
            then Nothing
            else defaultMuxerFor (codecType cfg)

selectSampleFormat :: AudioCodec -> Maybe Int -> SampleData -> (AVSampleFormat, Maybe Int)
selectSampleFormat CodecMp3 _ (SampleDataS16 _) = (avSampleFormatS16P, Nothing)
selectSampleFormat CodecMp3 _ (SampleDataS32 _) = (avSampleFormatS32P, Nothing)
selectSampleFormat CodecMp3 _ (SampleDataFloat _) = (avSampleFormatFLTP, Nothing)
selectSampleFormat CodecFlac (Just 16) (SampleDataS16 _) = (avSampleFormatS16, Just 16)
selectSampleFormat CodecFlac mDepth _ = (avSampleFormatS32, Just (fromMaybe 24 mDepth))
selectSampleFormat CodecOpus _ (SampleDataS16 _) = (avSampleFormatS16, Nothing)
selectSampleFormat CodecOpus _ _ = (avSampleFormatFLT, Nothing)
selectSampleFormat CodecAac _ _ = (avSampleFormatFLTP, Nothing)
selectSampleFormat CodecEac3 _ _ = (avSampleFormatFLTP, Nothing)
selectSampleFormat CodecPcmS16Le _ _ = (avSampleFormatS16, Just 16)
selectSampleFormat CodecPcmS24Le _ _ = (avSampleFormatS32, Just 24)

mp3FrameSize :: Int
mp3FrameSize = 1152

ptrSize :: Int
ptrSize = sizeOf (undefined :: Ptr ())

encodePcmToFile :: FilePath -> PCMBuffer -> IO ()
encodePcmToFile path buf = do
    let sampleRate = pcmSampleRate buf
        channels = pcmChannels buf
        samples = pcmSamples buf
        totalSamples = V.length samples `div` channels
        fs = mp3FrameSize

    codec <- findMp3Encoder
    withCodecContext codec sampleRate channels $ \ctx ->
        withOutputFile path codec ctx $ \fmt -> do
            let numFullFrames = totalSamples `div` fs
            mapM_ (sendFrameData ctx fmt samples channels sampleRate fs) [0 .. numFullFrames - 1]

            let remaining = totalSamples `mod` fs
            when (remaining > 0) $
                sendPartialFrame ctx fmt samples channels sampleRate fs
                    (numFullFrames * fs) remaining

            flushEncoder ctx fmt

encodeAudioToFile :: FilePath -> CodecConfig -> AudioBuffer -> IO ()
encodeAudioToFile path cfg buf = do
    let sampleRate = audioSampleRate buf
        layout = audioLayout buf
        channels = audioChannels buf
        samples = audioSamples buf
        totalFrames = audioFrameCount buf
    when (channels <= 0) $
        throwIO $ FFmpegError (-1) "encodeAudioToFile: invalid channel count"
    codec <- findEncoderByName (codecNameFor (codecType cfg))
    allowShort <- codecSupportsSmallLastFrame codec
    let (sampleFmt, bitsPerRaw) = selectSampleFormat (codecType cfg) (codecBitDepth cfg) samples
        opusOpts = case (codecType cfg, codecOpusMappingFamily cfg, layout) of
            (CodecOpus, Just fam, _) -> [("mapping_family", fromIntegral fam)]
            (CodecOpus, Nothing, LayoutAmbisonic _) -> [("mapping_family", 2)]
            _ -> []
        muxerName = resolveMuxerName path cfg
    withFormatContextWith muxerName path $ \fmt -> do
        setFormatStrictStdCompliance fmt (-2)
        needsGlobal <- formatNeedsGlobalHeader fmt
        let ccParams = CodecContextParams
                { ccSampleRate = sampleRate
                , ccChannelLayout = layout
                , ccSampleFormat = sampleFmt
                , ccBitRate = codecBitRate cfg
                , ccBitsPerRawSample = bitsPerRaw
                , ccGlobalHeader = needsGlobal
                , ccIntOptions = opusOpts
                }
        withCodecContextWith codec ccParams $ \ctx -> do
            sh <- createStream fmt codec
            codecTb <- getCodecTimeBase ctx
            setStreamTimeBase sh codecTb
            setCodecParameters sh ctx
            ctxFs <- getCodecFrameSize ctx
            let frameSize = if ctxFs > 0 then ctxFs else 1024
            bracket_
                (openOutput fmt path)
                (closeOutput fmt `catch` (\(_ :: SomeException) -> return ()))
                (do
                    writeHeader fmt
                    encodeAllFrames ctx fmt sh codecTb samples layout channels sampleRate sampleFmt frameSize allowShort totalFrames
                    writeTrailer fmt)

encodeAllFrames
    :: CodecContextHandle
    -> FormatContextHandle
    -> StreamHandle
    -> AVRational
    -> SampleData
    -> ChannelLayout
    -> Int
    -> Int
    -> AVSampleFormat
    -> Int
    -> Bool
    -> Int
    -> IO ()
encodeAllFrames ctx fmt sh codecTb samples layout channels sampleRate sampleFmt frameSize allowShort totalFrames = do
    let numFull = totalFrames `div` frameSize
        remFrames = totalFrames `mod` frameSize
    when (numFull > 0) $
        withFrameWithLayout frameSize sampleRate layout sampleFmt $ \fh -> do
            let loop idx
                    | idx >= numFull = return ()
                    | otherwise = do
                        makeFrameWritable fh
                        let startFrame = idx * frameSize
                        fillAudioFrame (getFramePtr fh) samples channels frameSize frameSize startFrame sampleFmt
                        pokeByteOff (castPtr (getFramePtr fh)) framePtsOffset (fromIntegral startFrame :: Int64)
                        sendFrameAndDrain ctx fmt sh codecTb fh
                        loop (idx + 1)
            loop 0
    when (remFrames > 0) $ do
        let allocSamples = if allowShort then remFrames else frameSize
            startFrame = numFull * frameSize
        withFrameWithLayout allocSamples sampleRate layout sampleFmt $ \fh -> do
            makeFrameWritable fh
            fillAudioFrame (getFramePtr fh) samples channels allocSamples remFrames startFrame sampleFmt
            pokeByteOff (castPtr (getFramePtr fh)) framePtsOffset (fromIntegral startFrame :: Int64)
            sendFrameAndDrain ctx fmt sh codecTb fh
    flushEncoderStream ctx fmt sh codecTb

clampFloat :: Float -> Float
clampFloat x
    | x > 1.0 = 1.0
    | x < -1.0 = -1.0
    | otherwise = x

floatToS16 :: Float -> Int16
floatToS16 x =
    let v = round (clampFloat x * 32767.0) :: Int
    in fromIntegral (max (-32768) (min 32767 v))

floatToS32 :: Float -> Int32
floatToS32 x =
    let v = round (clampFloat x * 8388607.0) :: Int
        clamped = max (-8388608) (min 8388607 v)
    in shiftL (fromIntegral clamped :: Int32) 8

s16ToS32 :: Int16 -> Int32
s16ToS32 x = shiftL (fromIntegral x :: Int32) 16

s16ToFloat :: Int16 -> Float
s16ToFloat x = fromIntegral x / 32768.0

s32ToS16 :: Int32 -> Int16
s32ToS16 x = fromIntegral (shiftR x 16)

s32ToFloat :: Int32 -> Float
s32ToFloat x = fromIntegral x / 2147483648.0

readSampleS16 :: SampleData -> Int -> Int16
readSampleS16 (SampleDataS16 v) i = VS.unsafeIndex v i
readSampleS16 (SampleDataS32 v) i = s32ToS16 (VS.unsafeIndex v i)
readSampleS16 (SampleDataFloat v) i = floatToS16 (VS.unsafeIndex v i)

readSampleS32 :: SampleData -> Int -> Int32
readSampleS32 (SampleDataS16 v) i = s16ToS32 (VS.unsafeIndex v i)
readSampleS32 (SampleDataS32 v) i = VS.unsafeIndex v i
readSampleS32 (SampleDataFloat v) i = floatToS32 (VS.unsafeIndex v i)

readSampleFloat :: SampleData -> Int -> Float
readSampleFloat (SampleDataS16 v) i = s16ToFloat (VS.unsafeIndex v i)
readSampleFloat (SampleDataS32 v) i = s32ToFloat (VS.unsafeIndex v i)
readSampleFloat (SampleDataFloat v) i = VS.unsafeIndex v i

fillAudioFrame :: Ptr AVFrame -> SampleData -> Int -> Int -> Int -> Int -> AVSampleFormat -> IO ()
fillAudioFrame framePtr samples channels nbSamples validSamples startFrame fmt = do
    extData <- peekByteOff (castPtr framePtr) frameExtendedDataOffset :: IO (Ptr (Ptr CUChar))
    let isPlanar = c_av_sample_fmt_is_planar fmt /= 0
        baseSampleIdx = startFrame * channels
    if isPlanar
        then fillPlanar extData baseSampleIdx
        else fillPacked extData baseSampleIdx
  where
    fillPlanar extData baseIdx
        | fmt == avSampleFormatS16P = goPlanar extData baseIdx (0 :: Int16) readSampleS16
        | fmt == avSampleFormatS32P = goPlanar extData baseIdx (0 :: Int32) readSampleS32
        | fmt == avSampleFormatFLTP = goPlanar extData baseIdx (0.0 :: Float) readSampleFloat
        | otherwise = throwIO $ FFmpegError (-1) ("Unsupported planar sample format: " ++ show fmt)

    fillPacked extData baseIdx
        | fmt == avSampleFormatS16 = goPacked extData baseIdx (0 :: Int16) readSampleS16
        | fmt == avSampleFormatS32 = goPacked extData baseIdx (0 :: Int32) readSampleS32
        | fmt == avSampleFormatFLT = goPacked extData baseIdx (0.0 :: Float) readSampleFloat
        | otherwise = throwIO $ FFmpegError (-1) ("Unsupported packed sample format: " ++ show fmt)

    goPlanar :: Storable a => Ptr (Ptr CUChar) -> Int -> a -> (SampleData -> Int -> a) -> IO ()
    goPlanar extData baseIdx zeroVal reader =
        let goCh c
                | c >= channels = return ()
                | otherwise = do
                    planeRaw <- peekElemOff extData c
                    let planePtr = castPtr planeRaw
                        loop i
                            | i >= nbSamples = return ()
                            | i < validSamples = do
                                pokeElemOff planePtr i (reader samples (baseIdx + i * channels + c))
                                loop (i + 1)
                            | otherwise = do
                                pokeElemOff planePtr i zeroVal
                                loop (i + 1)
                    loop 0
                    goCh (c + 1)
        in goCh 0

    goPacked :: Storable a => Ptr (Ptr CUChar) -> Int -> a -> (SampleData -> Int -> a) -> IO ()
    goPacked extData baseIdx zeroVal reader = do
        planeRaw <- peekElemOff extData 0
        let planePtr = castPtr planeRaw
            totalCount = nbSamples * channels
            validCount = validSamples * channels
            loop i
                | i >= totalCount = return ()
                | i < validCount = do
                    pokeElemOff planePtr i (reader samples (baseIdx + i))
                    loop (i + 1)
                | otherwise = do
                    pokeElemOff planePtr i zeroVal
                    loop (i + 1)
        loop 0

sendFrameAndDrain :: CodecContextHandle -> FormatContextHandle -> StreamHandle -> AVRational -> FrameHandle -> IO ()
sendFrameAndDrain ctx fmt sh codecTb fh = do
    let ctxPtr = getCodecContextPtr ctx
    ret <- c_avcodec_send_frame ctxPtr (getFramePtr fh)
    _ <- errorNonNegative "avcodec_send_frame" (fromIntegral ret)
    receivePacketsRescaled ctx fmt sh codecTb

receivePacketsRescaled :: CodecContextHandle -> FormatContextHandle -> StreamHandle -> AVRational -> IO ()
receivePacketsRescaled ctx fmt sh codecTb = do
    let ctxPtr = getCodecContextPtr ctx
    withPacket $ \ph -> do
        let loop = do
                ret <- c_avcodec_receive_packet ctxPtr (getPacketPtr ph)
                if ret == 0
                    then do
                        rescaleAndWriteFrame fmt sh codecTb ph
                        unrefPacket ph
                        loop
                    else if ret == averrorEagain
                        then return ()
                        else errorNonNegative "avcodec_receive_packet" (fromIntegral ret) >> return ()
        loop

flushEncoderStream :: CodecContextHandle -> FormatContextHandle -> StreamHandle -> AVRational -> IO ()
flushEncoderStream ctx fmt sh codecTb = do
    let ctxPtr = getCodecContextPtr ctx
    ret <- c_avcodec_send_frame ctxPtr nullPtr
    _ <- errorNonNegative "avcodec_send_frame (flush)" (fromIntegral ret)
    withPacket $ \ph -> do
        let loop = do
                ret' <- c_avcodec_receive_packet ctxPtr (getPacketPtr ph)
                if ret' == averrorEof
                    then return ()
                    else if ret' == averrorEagain
                        then return ()
                        else do
                            _ <- errorNonNegative "avcodec_receive_packet (flush)" (fromIntegral ret')
                            rescaleAndWriteFrame fmt sh codecTb ph
                            unrefPacket ph
                            loop
        loop

deinterleaveToFrame :: Ptr AVFrame -> V.Vector Int16 -> Int -> Int -> Int -> IO ()
deinterleaveToFrame frame samples channels nbSamples startOffset = do
    let go c = do
            let planeOffset = frameDataArrayOffset + c * ptrSize
            planePtr <- peekByteOff (castPtr frame) planeOffset :: IO (Ptr CUChar)
            let int16Plane = castPtr planePtr :: Ptr Int16
            let baseIdx = startOffset + c
            sequence_ [pokeElemOff int16Plane i (V.unsafeIndex samples (baseIdx + i * channels))
                      | i <- [0 .. nbSamples - 1]]
    mapM_ go [0 .. channels - 1]

sendFrameData :: CodecContextHandle -> FormatContextHandle -> V.Vector Int16 -> Int -> Int -> Int -> Int -> IO ()
sendFrameData ctx fmt samples channels sampleRate frameSize frameIdx =
    withFrame frameSize sampleRate channels avSampleFormatS16P $ \fh -> do
        makeFrameWritable fh
        let offset = frameIdx * frameSize * channels
        deinterleaveToFrame (getFramePtr fh) samples channels frameSize offset
        pokeByteOff (castPtr (getFramePtr fh)) framePtsOffset (fromIntegral (frameIdx * frameSize) :: Int64)
        doSendAndReceive ctx fmt fh

sendPartialFrame :: CodecContextHandle -> FormatContextHandle -> V.Vector Int16 -> Int -> Int -> Int -> Int -> Int -> IO ()
sendPartialFrame ctx fmt samples channels sampleRate _frameSize startFrame remaining =
    withFrame remaining sampleRate channels avSampleFormatS16P $ \fh -> do
        makeFrameWritable fh
        deinterleaveToFrame (getFramePtr fh) samples channels remaining (startFrame * channels)
        pokeByteOff (castPtr (getFramePtr fh)) framePtsOffset (fromIntegral startFrame :: Int64)
        doSendAndReceive ctx fmt fh

doSendAndReceive :: CodecContextHandle -> FormatContextHandle -> FrameHandle -> IO ()
doSendAndReceive ctx fmt fh = do
    let ctxPtr = getCodecContextPtr ctx
    ret <- c_avcodec_send_frame ctxPtr (getFramePtr fh)
    _ <- errorNonNegative "avcodec_send_frame" (fromIntegral ret)
    receivePackets ctx fmt

receivePackets :: CodecContextHandle -> FormatContextHandle -> IO ()
receivePackets ctx fmt = do
    let ctxPtr = getCodecContextPtr ctx
    withPacket $ \ph -> do
        let loop = do
                ret <- c_avcodec_receive_packet ctxPtr (getPacketPtr ph)
                if ret == 0
                    then do
                        writeFrame fmt ph
                        unrefPacket ph
                        loop
                    else if ret == averrorEagain
                        then return ()
                        else errorNonNegative "avcodec_receive_packet" (fromIntegral ret) >> return ()
        loop

flushEncoder :: CodecContextHandle -> FormatContextHandle -> IO ()
flushEncoder ctx fmt = do
    let ctxPtr = getCodecContextPtr ctx
    ret <- c_avcodec_send_frame ctxPtr nullPtr
    _ <- errorNonNegative "avcodec_send_frame (flush)" (fromIntegral ret)
    withPacket $ \ph -> do
        let loop = do
                ret' <- c_avcodec_receive_packet ctxPtr (getPacketPtr ph)
                if ret' == averrorEof
                    then return ()
                    else do
                        _ <- errorNonNegative "avcodec_receive_packet (flush)" (fromIntegral ret')
                        writeFrame fmt ph
                        unrefPacket ph
                        loop
        loop
