module IamfSpec (tests) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.List (isInfixOf, nub)
import Data.Vector.Storable qualified as VS
import FFmpeg.Audio.IAMF
import FFmpeg.Audio.Internal.Iamf
import FFmpeg.Audio.PCMBuffer
import Foreign.Ptr (nullPtr)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.IO (IOMode(ReadMode), hClose, hFileSize, openTempFile, withFile)
import System.Process (readProcessWithExitCode)

tests :: IO ()
tests = do
    testInternalIamfHandles
    testIamfStereoOpus
    testIamf7_1_4Flac24
    testIamfAmbisonicsFoaOpus
    testIamfMp4Opus
    testIamfBSNonEmpty
    putStrLn "All IAMF tests passed!"

makeTempPath :: String -> IO FilePath
makeTempPath ext = do
    (fp, h) <- openTempFile "/tmp" ("ffmpeg-iamf-test" ++ ext)
    hClose h
    return fp

sineFloat :: Int -> Int -> Int -> VS.Vector Float
sineFloat sr ch frames =
    VS.generate (frames * ch) $ \idx ->
        let f = idx `div` ch
            c = idx `mod` ch
            freq = 220.0 + fromIntegral c * 40.0 :: Double
            t = fromIntegral f / fromIntegral sr :: Double
        in realToFrac (0.2 * sin (2.0 * pi * freq * t))

assertNonEmpty :: FilePath -> String -> IO ()
assertNonEmpty fp label = do
    exists <- doesFileExist fp
    unless exists $ fail (label ++ ": file does not exist")
    sz <- withFile fp ReadMode hFileSize
    when (sz <= 0) $ fail (label ++ ": file is empty")
    putStrLn ("  " ++ label ++ " succeeded, size: " ++ show sz)

probeIamf :: FilePath -> String -> Int -> IO ()
probeIamf fp expectedCodec expectedStreams = do
    (ecG, outG, errG) <- readProcessWithExitCode "ffprobe"
        [ "-v", "error"
        , "-show_entries", "stream_group=type"
        , "-of", "default=noprint_wrappers=1"
        , fp
        ] ""
    case ecG of
        ExitFailure code -> fail ("ffprobe IAMF stream_groups failed (" ++ show code ++ "): " ++ errG)
        ExitSuccess -> do
            let lsG = lines outG
                hasAudioElem = any ("IAMF Audio Element" `isInfixOf`) lsG
                hasMixPres = any ("IAMF Mix Presentation" `isInfixOf`) lsG
            unless hasAudioElem $
                fail ("ffprobe missing IAMF Audio Element stream group:\n" ++ outG)
            unless hasMixPres $
                fail ("ffprobe missing IAMF Mix Presentation stream group:\n" ++ outG)
    (ecS, outS, errS) <- readProcessWithExitCode "ffprobe"
        [ "-v", "error"
        , "-show_entries", "stream=index,codec_name"
        , "-of", "csv=p=0"
        , fp
        ] ""
    case ecS of
        ExitFailure code -> fail ("ffprobe IAMF streams failed (" ++ show code ++ "): " ++ errS)
        ExitSuccess -> do
            let uniqueStreams = nub (filter (not . null) (lines outS))
                codecMatches = length (filter (("," ++ expectedCodec) `isInfixOf`) uniqueStreams)
            when (codecMatches /= expectedStreams) $
                fail ("Expected " ++ show expectedStreams ++ " " ++ expectedCodec
                    ++ " streams, found " ++ show codecMatches ++ ":\n" ++ outS)
            putStrLn ("    ffprobe verified IAMF: " ++ show codecMatches ++ "x "
                ++ expectedCodec ++ " streams + Audio Element + Mix Presentation")

testInternalIamfHandles :: IO ()
testInternalIamfHandles = do
    withIamfAudioElement $ \aeH -> do
        let aePtr = getIamfAudioElementPtr aeH
        when (aePtr == nullPtr) $ fail "allocateIamfAudioElement returned nullPtr"
        iamfAudioElementSetType aePtr avIamfAudioElementTypeChannel
        lPtr <- iamfAudioElementAddLayer aePtr
        iamfLayerSetChannelLayout lPtr Layout7_1_4
        nLayers <- iamfAudioElementNbLayers aePtr
        when (nLayers /= 1) $ fail ("Expected 1 layer, got " ++ show nLayers)
    withIamfMixPresentation $ \mpH -> do
        let mpPtr = getIamfMixPresentationPtr mpH
        when (mpPtr == nullPtr) $ fail "allocateIamfMixPresentation returned nullPtr"
        iamfMixPresentationSetAnnotation mpPtr "en-us" "Test Mix"
        smPtr <- iamfMixPresentationAddSubmix mpPtr
        _ <- iamfSubmixAddElement smPtr
        _ <- iamfSubmixAddLayout smPtr
        nSm <- iamfMixPresentationNbSubmixes mpPtr
        nEl <- iamfSubmixNbElements smPtr
        nLo <- iamfSubmixNbLayouts smPtr
        when (nSm /= 1 || nEl /= 1 || nLo /= 1) $
            fail ("Unexpected mix presentation counts: " ++ show (nSm, nEl, nLo))
    withIamfParamDefinition avIamfParamDefMixGain 2 $ \pdH -> do
        let pdPtr = getIamfParamDefinitionPtr pdH
        when (pdPtr == nullPtr) $ fail "allocateIamfParamDefinition returned nullPtr"
        sb0 <- iamfParamDefinitionGetSubblock pdPtr 0
        sb1 <- iamfParamDefinitionGetSubblock pdPtr 1
        when (sb0 == nullPtr || sb1 == nullPtr || sb0 == sb1) $
            fail "iamfParamDefinitionGetSubblock returned invalid subblock pointers"
    putStrLn "  Internal.Iamf handle lifecycle tests succeeded"

testIamfStereoOpus :: IO ()
testIamfStereoOpus = do
    fp <- makeTempPath ".iamf"
    let buf = AudioBuffer 48000 LayoutStereo (SampleDataFloat (sineFloat 48000 2 9600))
        el = IamfElement
            { iamfElementLabel = "Stereo Bed"
            , iamfElementKind = IamfChannelElement
            , iamfElementBuffer = buf
            , iamfElementGainDb = 0.0
            , iamfElementHeadphonesMode = IamfHeadphonesStereo
            }
    finally
        (do
            encodeIamf fp defaultIamfConfig [el]
            assertNonEmpty fp "encodeIamf stereo Opus (.iamf)"
            probeIamf fp "opus" 1)
        (removeFile fp)

testIamf7_1_4Flac24 :: IO ()
testIamf7_1_4Flac24 = do
    fp <- makeTempPath ".iamf"
    let buf = AudioBuffer 48000 Layout7_1_4 (SampleDataFloat (sineFloat 48000 12 9600))
        el = IamfElement
            { iamfElementLabel = "7.1.4 Bed"
            , iamfElementKind = IamfChannelElement
            , iamfElementBuffer = buf
            , iamfElementGainDb = 0.0
            , iamfElementHeadphonesMode = IamfHeadphonesBinaural
            }
        cfg = defaultIamfConfig
            { iamfCodec = IamfCodecFlac
            , iamfBitRatePerSubstream = Nothing
            , iamfBitDepth = Just 24
            , iamfTargetLayouts = [LayoutStereo, Layout7_1_4, LayoutBinaural]
            }
    finally
        (do
            encodeIamf fp cfg [el]
            assertNonEmpty fp "encodeIamf 7.1.4 24-bit FLAC (.iamf)"
            probeIamf fp "flac" 7)
        (removeFile fp)

testIamfAmbisonicsFoaOpus :: IO ()
testIamfAmbisonicsFoaOpus = do
    fp <- makeTempPath ".iamf"
    let buf = AudioBuffer 48000 (LayoutAmbisonic 1) (SampleDataFloat (sineFloat 48000 4 9600))
        el = IamfElement
            { iamfElementLabel = "FOA Scene"
            , iamfElementKind = IamfSceneElement
            , iamfElementBuffer = buf
            , iamfElementGainDb = 0.0
            , iamfElementHeadphonesMode = IamfHeadphonesBinaural
            }
        cfg = defaultIamfConfig
            { iamfCodec = IamfCodecOpus
            , iamfTargetLayouts = [LayoutStereo, LayoutBinaural]
            }
    finally
        (do
            encodeIamf fp cfg [el]
            assertNonEmpty fp "encodeIamf 4ch FOA Ambisonics Opus (.iamf)"
            probeIamf fp "opus" 4)
        (removeFile fp)

testIamfMp4Opus :: IO ()
testIamfMp4Opus = do
    fp <- makeTempPath ".mp4"
    let buf = AudioBuffer 48000 LayoutStereo (SampleDataFloat (sineFloat 48000 2 9600))
        el = IamfElement
            { iamfElementLabel = "Stereo Bed"
            , iamfElementKind = IamfChannelElement
            , iamfElementBuffer = buf
            , iamfElementGainDb = 0.0
            , iamfElementHeadphonesMode = IamfHeadphonesStereo
            }
        cfg = defaultIamfConfig
            { iamfContainer = IamfMp4
            , iamfCodec = IamfCodecOpus
            }
    finally
        (do
            encodeIamf fp cfg [el]
            assertNonEmpty fp "encodeIamf stereo Opus (.mp4)"
            probeIamf fp "opus" 1)
        (removeFile fp)

testIamfBSNonEmpty :: IO ()
testIamfBSNonEmpty = do
    let buf = AudioBuffer 48000 LayoutStereo (SampleDataFloat (sineFloat 48000 2 4800))
        el = IamfElement
            { iamfElementLabel = "Stereo Bed"
            , iamfElementKind = IamfChannelElement
            , iamfElementBuffer = buf
            , iamfElementGainDb = 0.0
            , iamfElementHeadphonesMode = IamfHeadphonesStereo
            }
    bytes <- encodeIamfBS defaultIamfConfig [el]
    when (BS.null bytes) $ fail "encodeIamfBS produced empty ByteString"
    putStrLn ("  encodeIamfBS succeeded, size: " ++ show (BS.length bytes))
