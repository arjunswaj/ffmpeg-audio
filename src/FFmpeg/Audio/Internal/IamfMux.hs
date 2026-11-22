module FFmpeg.Audio.Internal.IamfMux
    ( IamfContainer(..)
    , IamfSubstreamCodec(..)
    , IamfElementKind(..)
    , IamfHeadphonesMode(..)
    , IamfElement(..)
    , IamfConfig(..)
    , defaultIamfConfig
    , SubstreamSpec(..)
    , OpenedSubstream(..)
    , withIamfPrepared
    , encodeAllSubstreams
    , iamfTotalFrames
    , encodeIamfToFile
    , encodeIamfToBS
    ) where

import Control.Exception (SomeException, bracket_, catch, finally, mask_, throwIO)
import Control.Monad (forM, forM_, when)
import Control.Monad.ST (runST)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign.Storable (Storable, pokeByteOff)
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)

import FFmpeg.Audio.Internal.Codec
    ( CodecHandle
    , codecSupportsSmallLastFrame
    , findEncoderByName
    )
import FFmpeg.Audio.Internal.CodecContext
    ( CodecContextHandle
    , CodecContextParams(..)
    , getCodecFrameSize
    , getCodecTimeBase
    , withCodecContextWith
    )
import FFmpeg.Audio.Internal.Encoder
    ( AudioCodec(..)
    , codecNameFor
    , fillAudioFrame
    , flushEncoderStreamWith
    , selectSampleFormat
    , sendFrameAndDrainWith
    )
import FFmpeg.Audio.Internal.Packet (PacketHandle, withPacket)
import FFmpeg.Audio.Internal.Error (FFmpegError(..))
import FFmpeg.Audio.Internal.Format
    ( FormatContextHandle
    , StreamHandle
    , closeOutput
    , createStream
    , formatNeedsGlobalHeader
    , openOutput
    , setCodecParameters
    , setFormatStrictStdCompliance
    , setStreamId
    , setStreamTimeBase
    , withFormatContextWith
    , writeHeader
    , writeTrailer
    )
import FFmpeg.Audio.Internal.Frame
    ( FrameHandle
    , getFramePtr
    , makeFrameWritable
    , withFrameWithLayout
    )
import FFmpeg.Audio.Internal.Iamf
    ( allocRawMixGainParam
    , avIamfAmbisonicsModeMono
    , avIamfAudioElementTypeChannel
    , avIamfAudioElementTypeScene
    , avIamfHeadphonesModeBinaural
    , avIamfHeadphonesModeStereo
    , avIamfSubmixLayoutTypeBinaural
    , avIamfSubmixLayoutTypeLoudspeakers
    , iamfAudioElementAddLayer
    , iamfAudioElementSetType
    , iamfLayerSetAmbisonicsMode
    , iamfLayerSetChannelLayout
    , iamfMixPresentationAddSubmix
    , iamfMixPresentationSetAnnotation
    , iamfSubmixAddElement
    , iamfSubmixAddLayout
    , iamfSubmixElementSetAnnotation
    , iamfSubmixElementSetAudioElementId
    , iamfSubmixElementSetDefaultMixGain
    , iamfSubmixElementSetHeadphonesMode
    , iamfSubmixElementSetMixConfig
    , iamfSubmixLayoutSetLoudness
    , iamfSubmixLayoutSetSoundSystem
    , iamfSubmixLayoutSetType
    , iamfSubmixSetDefaultMixGain
    , iamfSubmixSetOutputMixConfig
    )
import FFmpeg.Audio.Internal.StreamGroup
    ( addStreamToGroup
    , avStreamGroupParamsIamfAudioElement
    , avStreamGroupParamsIamfMixPresentation
    , createStreamGroup
    , getStreamGroupAudioElement
    , getStreamGroupMixPresentation
    , setStreamGroupId
    )
import FFmpeg.Audio.Internal.Types
    ( AVRational(..)
    , AVSampleFormat
    , framePtsOffset
    )
import FFmpeg.Audio.PCMBuffer
    ( AudioBuffer(..)
    , ChannelLayout(..)
    , SampleData(..)
    , audioChannels
    , audioFrameCount
    )

data IamfContainer
    = IamfRaw
    | IamfMp4
    deriving (Show, Eq)

data IamfSubstreamCodec
    = IamfCodecOpus
    | IamfCodecFlac
    | IamfCodecAac
    | IamfCodecPcmS16Le
    | IamfCodecPcmS24Le
    deriving (Show, Eq)

data IamfElementKind
    = IamfChannelElement
    | IamfSceneElement
    deriving (Show, Eq)

data IamfHeadphonesMode
    = IamfHeadphonesStereo
    | IamfHeadphonesBinaural
    deriving (Show, Eq)

data IamfElement = IamfElement
    { iamfElementLabel          :: !String
    , iamfElementKind           :: !IamfElementKind
    , iamfElementBuffer         :: !AudioBuffer
    , iamfElementGainDb         :: !Double
    , iamfElementHeadphonesMode :: !IamfHeadphonesMode
    } deriving (Show, Eq)

data IamfConfig = IamfConfig
    { iamfContainer              :: !IamfContainer
    , iamfCodec                  :: !IamfSubstreamCodec
    , iamfSampleRate             :: !Int
    , iamfBitRatePerSubstream    :: !(Maybe Int)
    , iamfBitDepth               :: !(Maybe Int)
    , iamfMixLabel               :: !String
    , iamfLanguage               :: !String
    , iamfIntegratedLoudnessLkfs :: !Double
    , iamfTruePeakDbtp           :: !Double
    , iamfTargetLayouts          :: ![ChannelLayout]
    } deriving (Show, Eq)

defaultIamfConfig :: IamfConfig
defaultIamfConfig = IamfConfig
    { iamfContainer = IamfRaw
    , iamfCodec = IamfCodecOpus
    , iamfSampleRate = 48000
    , iamfBitRatePerSubstream = Just 128000
    , iamfBitDepth = Nothing
    , iamfMixLabel = "Main Mix"
    , iamfLanguage = "en-us"
    , iamfIntegratedLoudnessLkfs = -18.0
    , iamfTruePeakDbtp = -1.0
    , iamfTargetLayouts = [LayoutStereo, LayoutBinaural]
    }

data SubstreamSpec = SubstreamSpec
    { ssElemIndex :: !Int
    , ssStreamId  :: !Int
    , ssBuffer    :: !AudioBuffer
    , ssSampleFmt :: !AVSampleFormat
    , ssParams    :: !CodecContextParams
    }

data OpenedSubstream = OpenedSubstream
    { osSpec     :: !SubstreamSpec
    , osCtx      :: !CodecContextHandle
    , osStream   :: !StreamHandle
    , osTimeBase :: !AVRational
    }

toAudioCodec :: IamfSubstreamCodec -> AudioCodec
toAudioCodec IamfCodecOpus = CodecOpus
toAudioCodec IamfCodecFlac = CodecFlac
toAudioCodec IamfCodecAac = CodecAac
toAudioCodec IamfCodecPcmS16Le = CodecPcmS16Le
toAudioCodec IamfCodecPcmS24Le = CodecPcmS24Le

containerMuxer :: IamfContainer -> String
containerMuxer IamfRaw = "iamf"
containerMuxer IamfMp4 = "mp4"

containerExt :: IamfContainer -> String
containerExt IamfRaw = ".iamf"
containerExt IamfMp4 = ".mp4"

doubleToRational :: Double -> AVRational
doubleToRational x = AVRational (round (x * 256.0)) 256

extractChannels :: AudioBuffer -> [Int] -> AudioBuffer
extractChannels buf idxs =
    let !sr = audioSampleRate buf
        !srcCh = audioChannels buf
        !dstCh = length idxs
        !frames = audioFrameCount buf
        dstLayout = case dstCh of
            1 -> LayoutMono
            2 -> LayoutStereo
            n -> LayoutDefault n
        extractVec :: Storable a => VS.Vector a -> VS.Vector a
        extractVec v = case idxs of
            [c0] ->
                VS.generate frames (\f -> VS.unsafeIndex v (f * srcCh + c0))
            [c0, c1] -> runST $ do
                mv <- VSM.new (frames * 2)
                let go !f !srcOff !dstOff
                        | f >= frames = return ()
                        | otherwise = do
                            VSM.unsafeWrite mv dstOff (VS.unsafeIndex v (srcOff + c0))
                            VSM.unsafeWrite mv (dstOff + 1) (VS.unsafeIndex v (srcOff + c1))
                            go (f + 1) (srcOff + srcCh) (dstOff + 2)
                go 0 0 0
                VS.unsafeFreeze mv
            _ -> runST $ do
                let idxVec = VS.fromList idxs
                mv <- VSM.new (frames * dstCh)
                let go !f !srcOff !dstOff
                        | f >= frames = return ()
                        | otherwise = do
                            let goC !c
                                    | c >= dstCh = return ()
                                    | otherwise = do
                                        VSM.unsafeWrite mv (dstOff + c) (VS.unsafeIndex v (srcOff + VS.unsafeIndex idxVec c))
                                        goC (c + 1)
                            goC 0
                            go (f + 1) (srcOff + srcCh) (dstOff + dstCh)
                go 0 0 0
                VS.unsafeFreeze mv
        dstSamples = case audioSamples buf of
            SampleDataS16 v -> SampleDataS16 (extractVec v)
            SampleDataS32 v -> SampleDataS32 (extractVec v)
            SampleDataFloat v -> SampleDataFloat (extractVec v)
    in AudioBuffer sr dstLayout dstSamples

substreamChannelGroups :: IamfElement -> IO [[Int]]
substreamChannelGroups el =
    case (iamfElementKind el, audioLayout (iamfElementBuffer el)) of
        (IamfChannelElement, LayoutMono) -> return [[0]]
        (IamfChannelElement, LayoutStereo) -> return [[0, 1]]
        (IamfChannelElement, Layout5_1) -> return [[0, 1], [4, 5], [2], [3]]
        (IamfChannelElement, Layout7_1) -> return [[0, 1], [6, 7], [4, 5], [2], [3]]
        (IamfChannelElement, Layout7_1_2) -> return [[0, 1], [6, 7], [4, 5], [8, 9], [2], [3]]
        (IamfChannelElement, Layout7_1_4) -> return [[0, 1], [6, 7], [4, 5], [8, 9], [10, 11], [2], [3]]
        (IamfSceneElement, LayoutAmbisonic order)
            | order >= 1 ->
                let n = (order + 1) * (order + 1)
                in return [[c] | c <- [0 .. n - 1]]
        (kind, layout) ->
            throwIO $ FFmpegError (-1)
                ("Unsupported IAMF element combination: " ++ show kind ++ " with " ++ show layout)

withMany :: [a] -> (a -> (b -> IO r) -> IO r) -> ([b] -> IO r) -> IO r
withMany [] _ action = action []
withMany (x : xs) withOne action =
    withOne x $ \y ->
        withMany xs withOne (\ys -> action (y : ys))

iamfTotalFrames :: [OpenedSubstream] -> Int
iamfTotalFrames [] = 0
iamfTotalFrames opened = maximum (map (audioFrameCount . ssBuffer . osSpec) opened)

withIamfPrepared
    :: FormatContextHandle
    -> IamfConfig
    -> [IamfElement]
    -> (([OpenedSubstream], Int, Bool) -> IO a)
    -> IO a
withIamfPrepared fmt cfg elements action = do
    when (null elements) $
        throwIO $ FFmpegError (-1) "encodeIamf: at least one IamfElement is required"
    let sr = iamfSampleRate cfg
    forM_ elements $ \el ->
        when (audioSampleRate (iamfElementBuffer el) /= sr) $
            throwIO $ FFmpegError (-1)
                ("encodeIamf: element sample rate mismatch: expected " ++ show sr
                    ++ ", got " ++ show (audioSampleRate (iamfElementBuffer el)))
    let aCodec = toAudioCodec (iamfCodec cfg)
    codec <- findEncoderByName (codecNameFor aCodec)
    allowShort <- codecSupportsSmallLastFrame codec
    needsGlobal <- formatNeedsGlobalHeader fmt
    specs <- buildSubstreamSpecs cfg aCodec needsGlobal elements
    withMany specs (withCodecContextWith codec . ssParams) $ \ctxs -> do
        opened <- setupStreamsAndGroups fmt codec cfg elements specs ctxs
        frameSize <- resolveIamfFrameSize opened
        setupMixPresentation fmt cfg elements opened frameSize
        action (opened, frameSize, allowShort)

rawIamfEofPadObu :: BS.ByteString
rawIamfEofPadObu = BS.pack (0xf0 : 0x20 : replicate 32 0x00)

needsRawIamfEofPad :: IamfConfig -> Bool
needsRawIamfEofPad cfg =
    iamfContainer cfg == IamfRaw
        && (iamfCodec cfg /= IamfCodecOpus || maybe False (< 16000) (iamfBitRatePerSubstream cfg))

encodeIamfToFile :: FilePath -> IamfConfig -> [IamfElement] -> IO ()
encodeIamfToFile path cfg elements = do
    let sr = iamfSampleRate cfg
    withFormatContextWith (Just (containerMuxer (iamfContainer cfg))) path $ \fmt -> do
        setFormatStrictStdCompliance fmt (-2)
        withIamfPrepared fmt cfg elements $ \(opened, frameSize, allowShort) ->
            bracket_
                (openOutput fmt path)
                (closeOutput fmt `catch` (\(_ :: SomeException) -> return ()))
                (do
                    writeHeader fmt
                    encodeAllSubstreams fmt opened sr frameSize allowShort
                    writeTrailer fmt)
    when (needsRawIamfEofPad cfg) $
        BS.appendFile path rawIamfEofPadObu

buildSubstreamSpecs
    :: IamfConfig
    -> AudioCodec
    -> Bool
    -> [IamfElement]
    -> IO [SubstreamSpec]
buildSubstreamSpecs cfg aCodec needsGlobal elements = do
    grouped <- forM (zip [0 ..] elements) $ \(eIdx, el) -> do
        groups <- substreamChannelGroups el
        return [(eIdx, extractChannels (iamfElementBuffer el) grp) | grp <- groups]
    let flat = concat grouped
        sr = iamfSampleRate cfg
        opusOpts = case iamfCodec cfg of
            IamfCodecOpus -> [("vbr", 0)]
            _ -> []
    return
        [ let (sFmt, bitsPerRaw) = selectSampleFormat aCodec (iamfBitDepth cfg) (audioSamples subBuf)
              ccParams = CodecContextParams
                  { ccSampleRate = sr
                  , ccChannelLayout = audioLayout subBuf
                  , ccSampleFormat = sFmt
                  , ccBitRate = iamfBitRatePerSubstream cfg
                  , ccBitsPerRawSample = bitsPerRaw
                  , ccGlobalHeader = needsGlobal
                  , ccIntOptions = opusOpts
                  }
          in SubstreamSpec
              { ssElemIndex = eIdx
              , ssStreamId = sId
              , ssBuffer = subBuf
              , ssSampleFmt = sFmt
              , ssParams = ccParams
              }
        | (sId, (eIdx, subBuf)) <- zip [1 ..] flat
        ]

setupStreamsAndGroups
    :: FormatContextHandle
    -> CodecHandle
    -> IamfConfig
    -> [IamfElement]
    -> [SubstreamSpec]
    -> [CodecContextHandle]
    -> IO [OpenedSubstream]
setupStreamsAndGroups fmt codec _cfg elements specs ctxs = do
    aeGroups <- forM (zip [0 ..] elements) $ \(eIdx, el) -> do
        sg <- createStreamGroup fmt avStreamGroupParamsIamfAudioElement
        setStreamGroupId sg (fromIntegral (eIdx + 1))
        aePtr <- getStreamGroupAudioElement sg
        case iamfElementKind el of
            IamfChannelElement -> do
                iamfAudioElementSetType aePtr avIamfAudioElementTypeChannel
                layerPtr <- iamfAudioElementAddLayer aePtr
                iamfLayerSetChannelLayout layerPtr (audioLayout (iamfElementBuffer el))
            IamfSceneElement -> do
                iamfAudioElementSetType aePtr avIamfAudioElementTypeScene
                layerPtr <- iamfAudioElementAddLayer aePtr
                iamfLayerSetChannelLayout layerPtr (audioLayout (iamfElementBuffer el))
                iamfLayerSetAmbisonicsMode layerPtr avIamfAmbisonicsModeMono
        return (eIdx, sg)
    forM (zip specs ctxs) $ \(sp, ctx) -> do
        sh <- createStream fmt codec
        setStreamId sh (ssStreamId sp)
        codecTb <- getCodecTimeBase ctx
        setStreamTimeBase sh codecTb
        setCodecParameters sh ctx
        case lookup (ssElemIndex sp) aeGroups of
            Just sg -> addStreamToGroup sg sh
            Nothing -> throwIO $ FFmpegError (-1) "Internal error: missing audio element stream group"
        return OpenedSubstream
            { osSpec = sp
            , osCtx = ctx
            , osStream = sh
            , osTimeBase = codecTb
            }

resolveIamfFrameSize :: [OpenedSubstream] -> IO Int
resolveIamfFrameSize [] = return 960
resolveIamfFrameSize (os : _) = do
    fs <- getCodecFrameSize (osCtx os)
    return (if fs > 0 then fs else 960)

setupMixPresentation
    :: FormatContextHandle
    -> IamfConfig
    -> [IamfElement]
    -> [OpenedSubstream]
    -> Int
    -> IO ()
setupMixPresentation fmt cfg elements opened frameSize = do
    let sr = iamfSampleRate cfg
    mpGroup <- createStreamGroup fmt avStreamGroupParamsIamfMixPresentation
    setStreamGroupId mpGroup 1
    forM_ opened $ \os -> addStreamToGroup mpGroup (osStream os)
    mpPtr <- getStreamGroupMixPresentation mpGroup
    iamfMixPresentationSetAnnotation mpPtr (iamfLanguage cfg) (iamfMixLabel cfg)
    submixPtr <- iamfMixPresentationAddSubmix mpPtr
    iamfSubmixSetDefaultMixGain submixPtr (AVRational 0 1)
    mask_ $ do
        outParam <- allocRawMixGainParam 1 (fromIntegral sr) (fromIntegral frameSize) (AVRational 0 1)
        iamfSubmixSetOutputMixConfig submixPtr outParam
    forM_ (zip ([0 ..] :: [Int]) elements) $ \(eIdx, el) -> do
        sePtr <- iamfSubmixAddElement submixPtr
        let aeId = fromIntegral (eIdx + 1)
            paramId = fromIntegral (100 + eIdx)
            gainRat = doubleToRational (iamfElementGainDb el)
            hpMode = case iamfElementHeadphonesMode el of
                IamfHeadphonesStereo -> avIamfHeadphonesModeStereo
                IamfHeadphonesBinaural -> avIamfHeadphonesModeBinaural
        iamfSubmixElementSetAudioElementId sePtr aeId
        iamfSubmixElementSetDefaultMixGain sePtr gainRat
        iamfSubmixElementSetHeadphonesMode sePtr hpMode
        iamfSubmixElementSetAnnotation sePtr (iamfLanguage cfg) (iamfElementLabel el)
        mask_ $ do
            elParam <- allocRawMixGainParam paramId (fromIntegral sr) (fromIntegral frameSize) gainRat
            iamfSubmixElementSetMixConfig sePtr elParam
    let targets = if null (iamfTargetLayouts cfg) then [LayoutStereo] else iamfTargetLayouts cfg
        intLoud = doubleToRational (iamfIntegratedLoudnessLkfs cfg)
        peakVal = doubleToRational (iamfTruePeakDbtp cfg)
    forM_ targets $ \layout -> do
        loPtr <- iamfSubmixAddLayout submixPtr
        if layout == LayoutBinaural
            then
                iamfSubmixLayoutSetType loPtr avIamfSubmixLayoutTypeBinaural
            else do
                iamfSubmixLayoutSetType loPtr avIamfSubmixLayoutTypeLoudspeakers
                iamfSubmixLayoutSetSoundSystem loPtr layout
        iamfSubmixLayoutSetLoudness loPtr intLoud peakVal peakVal

encodeAllSubstreams
    :: FormatContextHandle
    -> [OpenedSubstream]
    -> Int
    -> Int
    -> Bool
    -> IO ()
encodeAllSubstreams fmt opened sr frameSize allowShort =
    withPacket $ \ph -> do
        let !maxFrames = iamfTotalFrames opened
            !numFull = maxFrames `div` frameSize
            !remFrames = maxFrames `mod` frameSize
        when (numFull > 0) $
            withMany opened (\os -> withFrameWithLayout frameSize sr (audioLayout (ssBuffer (osSpec os))) (ssSampleFmt (osSpec os))) $ \frames -> do
                let pairs = zip opened frames
                    loop !idx
                        | idx >= numFull = return ()
                        | otherwise = do
                            let !startFrame = idx * frameSize
                            forM_ pairs $ \(os, fh) ->
                                sendSubstreamChunk fmt os fh ph frameSize startFrame
                            loop (idx + 1)
                loop 0
        when (remFrames > 0) $ do
            let !allocSamples = if allowShort then remFrames else frameSize
                !startFrame = numFull * frameSize
            forM_ opened $ \os ->
                withFrameWithLayout allocSamples sr (audioLayout (ssBuffer (osSpec os))) (ssSampleFmt (osSpec os)) $ \fh ->
                    sendSubstreamChunk fmt os fh ph allocSamples startFrame
        forM_ opened $ \os ->
            flushEncoderStreamWith (osCtx os) fmt (osStream os) (osTimeBase os) ph

sendSubstreamChunk
    :: FormatContextHandle
    -> OpenedSubstream
    -> FrameHandle
    -> PacketHandle
    -> Int
    -> Int
    -> IO ()
sendSubstreamChunk fmt os fh ph allocSamples startFrame = do
    let buf = ssBuffer (osSpec os)
        !total = audioFrameCount buf
        !valid = max 0 (min allocSamples (total - startFrame))
    makeFrameWritable fh
    fillAudioFrame
        (getFramePtr fh)
        (audioSamples buf)
        (audioChannels buf)
        allocSamples
        valid
        startFrame
        (ssSampleFmt (osSpec os))
    pokeByteOff (getFramePtr fh) framePtsOffset (fromIntegral startFrame :: Int64)
    sendFrameAndDrainWith (osCtx os) fmt (osStream os) (osTimeBase os) fh ph

encodeIamfToBS :: IamfConfig -> [IamfElement] -> IO BS.ByteString
encodeIamfToBS cfg elements = do
    (path, h) <- openTempFile "/tmp" ("ffmpeg-audio" ++ containerExt (iamfContainer cfg))
    hClose h
    finally
        (encodeIamfToFile path cfg elements >> BS.readFile path)
        (removeFile path `catch` (\(_ :: SomeException) -> return ()))
