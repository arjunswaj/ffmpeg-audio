module Mp4Spec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_, unless, when)
import Data.Bits ((.|.), shiftL)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Vector.Storable qualified as VS
import Data.Word (Word32)
import FFmpeg.Audio
import FFmpeg.Audio.Internal.Image (decodeCoverToYuv420, yuvHeight, yuvWidth)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.IO (IOMode(ReadMode), hClose, hFileSize, openTempFile, withFile)
import System.Mem (performGC)
import System.Process (readProcessWithExitCode)

tests :: IO ()
tests = do
    testSpatialMp4DefaultCoverAndLyrics
    testSpatialMp4JpegAndPngCovers
    testEclipsaMp4FoaScene
    testEclipsaMp4Bed7_1_2
    testSurroundM4a
    testCoverAndVideo160FramesPerf
    testRepeatedEncodeMemoryStability
    putStrLn "All MP4 / Eclipsa / Surround M4A tests passed!"

makeTempPath :: String -> IO FilePath
makeTempPath ext = do
    (fp, h) <- openTempFile "/tmp" ("ffmpeg-mp4-test" ++ ext)
    hClose h
    return fp

sineBuffer :: Int -> ChannelLayout -> Int -> AudioBuffer
sineBuffer sr layout frames =
    let ch = channelLayoutCount layout
        vec = VS.generate (frames * ch) $ \idx ->
            let f = idx `div` ch
                c = idx `mod` ch
                freq = 220.0 + fromIntegral c * 55.0 :: Double
                t = fromIntegral f / fromIntegral sr :: Double
            in realToFrac (0.2 * sin (2.0 * pi * freq * t))
    in AudioBuffer sr layout (SampleDataFloat vec)

assertNonEmpty :: FilePath -> String -> IO ()
assertNonEmpty fp label = do
    exists <- doesFileExist fp
    unless exists $ fail (label ++ ": file does not exist")
    sz <- withFile fp ReadMode hFileSize
    when (sz <= 0) $ fail (label ++ ": file is empty")
    putStrLn ("  " ++ label ++ " succeeded, size: " ++ show sz)

assertFfmpegDecodeClean :: FilePath -> String -> IO ()
assertFfmpegDecodeClean fp label = do
    (ec, _, err) <- readProcessWithExitCode "ffmpeg"
        [ "-v", "error"
        , "-i", fp
        , "-f", "null"
        , "-"
        ] ""
    case ec of
        ExitFailure code -> fail (label ++ ": ffmpeg decode failed (" ++ show code ++ "): " ++ err)
        ExitSuccess -> do
            unless (null err) $
                fail (label ++ ": ffmpeg decode emitted stderr:\n" ++ err)
            putStrLn ("    ffmpeg -v error decode clean for " ++ label)

readBeWord32 :: BS.ByteString -> Int -> Word32
readBeWord32 bs off =
    (fromIntegral (BS.index bs off) `shiftL` 24)
    .|. (fromIntegral (BS.index bs (off + 1)) `shiftL` 16)
    .|. (fromIntegral (BS.index bs (off + 2)) `shiftL` 8)
    .|. fromIntegral (BS.index bs (off + 3))

topLevelMp4Boxes :: BS.ByteString -> [(String, Int)]
topLevelMp4Boxes bs = go 0
  where
    n = BS.length bs
    go off
        | off + 8 > n = []
        | otherwise =
            let sz32 = fromIntegral (readBeWord32 bs off) :: Int
                fourcc = map (chr . fromIntegral) (BS.unpack (BS.take 4 (BS.drop (off + 4) bs)))
                step
                    | sz32 == 0 = n - off
                    | sz32 == 1 && off + 16 <= n =
                        let hi = fromIntegral (readBeWord32 bs (off + 8)) :: Int
                            lo = fromIntegral (readBeWord32 bs (off + 12)) :: Int
                        in max 16 ((hi `shiftL` 32) .|. lo)
                    | sz32 < 8 = n - off
                    | otherwise = sz32
            in (fourcc, off) : go (off + step)

assertMoovBeforeMdat :: FilePath -> String -> IO ()
assertMoovBeforeMdat fp label = do
    bs <- BS.readFile fp
    let boxes = topLevelMp4Boxes bs
        mMoov = lookup "moov" boxes
        mMdat = lookup "mdat" boxes
    case (mMoov, mMdat) of
        (Just moovOff, Just mdatOff) -> do
            when (moovOff >= mdatOff) $
                fail (label ++ ": expected faststart moov before mdat, got moov=" ++ show moovOff ++ ", mdat=" ++ show mdatOff)
            putStrLn ("    faststart verified (moov@" ++ show moovOff ++ " < mdat@" ++ show mdatOff ++ ")")
        _ -> fail (label ++ ": missing moov or mdat box in " ++ show boxes)

generateTestImage :: String -> String -> String -> IO BS.ByteString
generateTestImage ext sizeSpec pixFmt = do
    fp <- makeTempPath ext
    finally
        (do
            (ec, _, err) <- readProcessWithExitCode "ffmpeg"
                [ "-y", "-v", "error"
                , "-f", "lavfi"
                , "-i", "color=c=0x336699:size=" ++ sizeSpec ++ ":rate=1:duration=1,format=" ++ pixFmt
                , "-frames:v", "1"
                , fp
                ] ""
            case ec of
                ExitFailure c -> fail ("Failed generating test image " ++ ext ++ " (" ++ show c ++ "): " ++ err)
                ExitSuccess -> BS.readFile fp)
        (removeFile fp)

testSpatialMp4DefaultCoverAndLyrics :: IO ()
testSpatialMp4DefaultCoverAndLyrics = do
    fp <- makeTempPath ".mp4"
    let binBuf = sineBuffer 48000 LayoutBinaural 96000
        surBuf = sineBuffer 48000 Layout5_1 96000
        meta = AudioMetadata
            { metaTitle = Just "Spatial Dawn"
            , metaArtist = Just "Antigravity Studio"
            , metaAlbum = Just "Immersive Album"
            , metaSyncedLyrics =
                [ TimedLyric 250 "Opening line"
                , TimedLyric 1100 "Second line \8212 \2360\2369\2352"
                ]
            , metaUnsyncedLyrics = Just "Opening line\nSecond line \8212 \2360\2369\2352"
            }
        cfg = defaultSpatialMp4Config { smp4Metadata = meta }
    finally
        (do
            encodeSpatialMp4 fp cfg binBuf surBuf
            assertNonEmpty fp "encodeSpatialMp4 (default cover + binaural + 5.1 + mov_text)"
            assertMoovBeforeMdat fp "encodeSpatialMp4"
            assertFfmpegDecodeClean fp "encodeSpatialMp4"
            (ec, out, err) <- readProcessWithExitCode "ffprobe"
                [ "-v", "error"
                , "-show_streams"
                , "-show_format"
                , fp
                ] ""
            case ec of
                ExitFailure c -> fail ("ffprobe Spatial MP4 failed (" ++ show c ++ "): " ++ err)
                ExitSuccess -> do
                    unless ("codec_name=h264" `isInfixOf` out && "width=1280" `isInfixOf` out && "height=720" `isInfixOf` out && "pix_fmt=yuv420p" `isInfixOf` out) $
                        fail ("Spatial MP4 missing expected 1280x720 yuv420p H.264 stream:\n" ++ out)
                    unless ("channel_layout=stereo" `isInfixOf` out && "Binaural 3D (Headphones)" `isInfixOf` out) $
                        fail ("Spatial MP4 missing Binaural stereo track:\n" ++ out)
                    unless ("channels=6" `isInfixOf` out && "channel_layout=5.1" `isInfixOf` out && "5.1 Surround" `isInfixOf` out) $
                        fail ("Spatial MP4 missing 5.1 Surround track:\n" ++ out)
                    unless ("codec_name=mov_text" `isInfixOf` out && "codec_type=subtitle" `isInfixOf` out) $
                        fail ("Spatial MP4 missing mov_text subtitle stream:\n" ++ out)
                    unless ("TAG:title=Spatial Dawn" `isInfixOf` out && "TAG:artist=Antigravity Studio" `isInfixOf` out && "TAG:album=Immersive Album" `isInfixOf` out) $
                        fail ("Spatial MP4 missing format metadata tags:\n" ++ out)
            (ecSub, outSub, errSub) <- readProcessWithExitCode "ffprobe"
                [ "-v", "error"
                , "-select_streams", "s:0"
                , "-show_packets"
                , "-show_data"
                , fp
                ] ""
            case ecSub of
                ExitFailure c -> fail ("ffprobe Spatial MP4 subtitles failed (" ++ show c ++ "): " ++ errSub)
                ExitSuccess ->
                    unless ("Opening line" `isInfixOf` outSub) $
                        fail ("Spatial MP4 mov_text missing lyric payload:\n" ++ outSub)
            bs <- encodeSpatialMp4BS cfg binBuf surBuf
            when (BS.null bs) $ fail "encodeSpatialMp4BS returned empty ByteString"
            putStrLn ("    ffprobe verified H.264 + Binaural AAC + 5.1 AAC + mov_text + metadata (BS=" ++ show (BS.length bs) ++ ")"))
        (removeFile fp)

testSpatialMp4JpegAndPngCovers :: IO ()
testSpatialMp4JpegAndPngCovers = do
    jpgBytes <- generateTestImage ".jpg" "320x240" "yuvj420p"
    pngBytes <- generateTestImage ".png" "321x241" "rgba"
    let binBuf = sineBuffer 48000 LayoutStereo 48000
        surBuf = sineBuffer 48000 Layout5_1 48000
    fpJpg <- makeTempPath ".mp4"
    finally
        (do
            encodeSpatialMp4 fpJpg (defaultSpatialMp4Config { smp4CoverImage = Just jpgBytes }) binBuf surBuf
            assertNonEmpty fpJpg "encodeSpatialMp4 (JPEG 320x240 cover)"
            assertMoovBeforeMdat fpJpg "encodeSpatialMp4 JPEG"
            assertFfmpegDecodeClean fpJpg "encodeSpatialMp4 JPEG"
            (ec, out, err) <- readProcessWithExitCode "ffprobe"
                [ "-v", "error"
                , "-select_streams", "v:0"
                , "-show_entries", "stream=codec_name,width,height,pix_fmt"
                , "-of", "csv=p=0"
                , fpJpg
                ] ""
            case ec of
                ExitFailure c -> fail ("ffprobe JPEG cover failed (" ++ show c ++ "): " ++ err)
                ExitSuccess ->
                    unless ("h264,320,240,yuv420p" `isInfixOf` out) $
                        fail ("Expected h264,320,240,yuv420p, got: " ++ out))
        (removeFile fpJpg)
    fpPng <- makeTempPath ".mp4"
    finally
        (do
            encodeSpatialMp4 fpPng (defaultSpatialMp4Config { smp4CoverImage = Just pngBytes }) binBuf surBuf
            assertNonEmpty fpPng "encodeSpatialMp4 (odd RGBA PNG 321x241 cover)"
            assertMoovBeforeMdat fpPng "encodeSpatialMp4 PNG"
            assertFfmpegDecodeClean fpPng "encodeSpatialMp4 PNG"
            (ec, out, err) <- readProcessWithExitCode "ffprobe"
                [ "-v", "error"
                , "-select_streams", "v:0"
                , "-show_entries", "stream=codec_name,width,height,pix_fmt"
                , "-of", "csv=p=0"
                , fpPng
                ] ""
            case ec of
                ExitFailure c -> fail ("ffprobe PNG cover failed (" ++ show c ++ "): " ++ err)
                ExitSuccess ->
                    unless ("h264,322,242,yuv420p" `isInfixOf` out) $
                        fail ("Expected odd PNG resized to even h264,322,242,yuv420p, got: " ++ out))
        (removeFile fpPng)

probeEclipsaMp4 :: FilePath -> Int -> String -> IO ()
probeEclipsaMp4 fp expectedOpusStreams label = do
    (ecG, outG, errG) <- readProcessWithExitCode "ffprobe"
        [ "-v", "error"
        , "-show_entries", "stream_group=type"
        , "-of", "default=noprint_wrappers=1"
        , fp
        ] ""
    case ecG of
        ExitFailure c -> fail (label ++ " ffprobe stream_group failed (" ++ show c ++ "): " ++ errG)
        ExitSuccess -> do
            unless ("IAMF Audio Element" `isInfixOf` outG) $
                fail (label ++ ": missing IAMF Audio Element:\n" ++ outG)
            unless ("IAMF Mix Presentation" `isInfixOf` outG) $
                fail (label ++ ": missing IAMF Mix Presentation:\n" ++ outG)
    (ecS, outS, errS) <- readProcessWithExitCode "ffprobe"
        [ "-v", "error"
        , "-show_entries", "stream=index,codec_type,codec_name"
        , "-of", "csv=p=0"
        , fp
        ] ""
    case ecS of
        ExitFailure c -> fail (label ++ " ffprobe streams failed (" ++ show c ++ "): " ++ errS)
        ExitSuccess -> do
            let ls = nub (filter (not . null) (lines outS))
                opusCount = length (filter (",opus" `isInfixOf`) ls)
                h264Count = length (filter (",h264" `isInfixOf`) ls)
                hasAac = any (",aac" `isInfixOf`) ls
            when (opusCount /= expectedOpusStreams) $
                fail (label ++ ": expected " ++ show expectedOpusStreams ++ " opus streams, got " ++ show opusCount ++ ":\n" ++ outS)
            when (h264Count /= 1) $
                fail (label ++ ": expected 1 h264 video stream, got " ++ show h264Count ++ ":\n" ++ outS)
            when hasAac $
                fail (label ++ ": Eclipsa MP4 must be IAMF-only with no competing AAC track:\n" ++ outS)
            putStrLn ("    ffprobe verified Eclipsa MP4: " ++ show opusCount ++ "x IAMF Opus + 1x H.264 (no legacy AAC)")

testEclipsaMp4FoaScene :: IO ()
testEclipsaMp4FoaScene = do
    fp <- makeTempPath ".mp4"
    jpgBytes <- generateTestImage ".jpg" "320x240" "yuvj420p"
    let foaBuf = sineBuffer 48000 (LayoutAmbisonic 1) 96000
        el = IamfElement
            { iamfElementLabel = "FOA Scene"
            , iamfElementKind = IamfSceneElement
            , iamfElementBuffer = foaBuf
            , iamfElementGainDb = 0.0
            , iamfElementHeadphonesMode = IamfHeadphonesBinaural
            }
        meta = emptyAudioMetadata
            { metaTitle = Just "Eclipsa FOA"
            , metaArtist = Just "Antigravity"
            , metaAlbum = Just "Eclipsa Sessions"
            }
        cfg = defaultEclipsaMp4Config
            { emp4CoverImage = Just jpgBytes
            , emp4Metadata = meta
            }
    finally
        (do
            encodeEclipsaMp4 fp cfg [el]
            assertNonEmpty fp "encodeEclipsaMp4 (FOA Scene + H.264 cover)"
            assertMoovBeforeMdat fp "encodeEclipsaMp4 FOA"
            assertFfmpegDecodeClean fp "encodeEclipsaMp4 FOA"
            probeEclipsaMp4 fp 4 "encodeEclipsaMp4 FOA"
            bs <- encodeEclipsaMp4BS cfg [el]
            when (BS.null bs) $ fail "encodeEclipsaMp4BS returned empty ByteString")
        (removeFile fp)

testEclipsaMp4Bed7_1_2 :: IO ()
testEclipsaMp4Bed7_1_2 = do
    fp <- makeTempPath ".mp4"
    let bedBuf = sineBuffer 48000 Layout7_1_2 48000
        el = IamfElement
            { iamfElementLabel = "7.1.2 Bed"
            , iamfElementKind = IamfChannelElement
            , iamfElementBuffer = bedBuf
            , iamfElementGainDb = 0.0
            , iamfElementHeadphonesMode = IamfHeadphonesBinaural
            }
        cfg = defaultEclipsaMp4Config
            { emp4Metadata = emptyAudioMetadata { metaTitle = Just "Eclipsa 7.1.2 Bed" }
            }
    finally
        (do
            encodeEclipsaMp4 fp cfg [el]
            assertNonEmpty fp "encodeEclipsaMp4 (7.1.2 Bed + default dark slate cover)"
            assertMoovBeforeMdat fp "encodeEclipsaMp4 7.1.2"
            assertFfmpegDecodeClean fp "encodeEclipsaMp4 7.1.2"
            probeEclipsaMp4 fp 6 "encodeEclipsaMp4 7.1.2")
        (removeFile fp)

testSurroundM4a :: IO ()
testSurroundM4a = do
    fp <- makeTempPath ".m4a"
    let surBuf = sineBuffer 48000 Layout5_1 48000
        meta = AudioMetadata
            { metaTitle = Just "Surround Night"
            , metaArtist = Just "Antigravity"
            , metaAlbum = Just "Plexamp 5.1"
            , metaSyncedLyrics = [TimedLyric 0 "Night falls softly"]
            , metaUnsyncedLyrics = Just "Night falls softly"
            }
        cfg = defaultSurroundM4aConfig { sm4aMetadata = meta }
    finally
        (do
            encodeSurroundM4a fp cfg surBuf
            assertNonEmpty fp "encodeSurroundM4a (5.1 AAC M4A)"
            assertMoovBeforeMdat fp "encodeSurroundM4a"
            assertFfmpegDecodeClean fp "encodeSurroundM4a"
            (ec, out, err) <- readProcessWithExitCode "ffprobe"
                [ "-v", "error"
                , "-show_streams"
                , "-show_format"
                , fp
                ] ""
            case ec of
                ExitFailure c -> fail ("ffprobe Surround M4A failed (" ++ show c ++ "): " ++ err)
                ExitSuccess -> do
                    unless ("codec_name=aac" `isInfixOf` out && "channels=6" `isInfixOf` out && "channel_layout=5.1" `isInfixOf` out && "sample_rate=48000" `isInfixOf` out) $
                        fail ("Surround M4A missing expected 6ch 5.1 48kHz AAC stream:\n" ++ out)
                    when ("codec_type=video" `isInfixOf` out) $
                        fail ("Surround M4A should be audio-only:\n" ++ out)
                    unless ("TAG:title=Surround Night" `isInfixOf` out && "TAG:artist=Antigravity" `isInfixOf` out && "TAG:album=Plexamp 5.1" `isInfixOf` out && "TAG:lyrics=Night falls softly" `isInfixOf` out) $
                        fail ("Surround M4A missing format metadata tags:\n" ++ out)
            bs <- encodeSurroundM4aBS cfg surBuf
            when (BS.null bs) $ fail "encodeSurroundM4aBS returned empty ByteString"
            putStrLn ("    ffprobe verified 5.1 AAC M4A + metadata + faststart (BS=" ++ show (BS.length bs) ++ ")"))
        (removeFile fp)

readVmRssKb :: IO Int
readVmRssKb = do
    status <- readFile "/proc/self/status"
    case filter ("VmRSS:" `isPrefixOf`) (lines status) of
        (line : _) ->
            case words line of
                (_ : kbStr : _) -> return (read kbStr)
                _ -> return 0
        [] -> return 0

elapsedMsSince :: Word -> IO Double
elapsedMsSince t0 = do
    t1 <- fromIntegral <$> getMonotonicTimeNSec
    return (fromIntegral (t1 - t0) / 1.0e6)

testCoverAndVideo160FramesPerf :: IO ()
testCoverAndVideo160FramesPerf = do
    hdPng <- generateTestImage ".png" "1281x721" "rgba"
    tImg0 <- fromIntegral <$> getMonotonicTimeNSec
    img <- decodeCoverToYuv420 hdPng
    imgMs <- elapsedMsSince tImg0
    when (yuvWidth img /= 1282 || yuvHeight img /= 722) $
        fail ("Unexpected resized HD PNG dims: " ++ show (yuvWidth img, yuvHeight img))
    putStrLn ("  HD odd RGBA PNG (1281x721 -> 1282x722) decode+bilinear+BT.709: " ++ show (round imgMs :: Int) ++ " ms")
    when (imgMs > 1000.0) $
        fail ("Cover decode+resize took too long: " ++ show imgMs ++ " ms")
    let binBuf = sineBuffer 48000 LayoutStereo 48000
        surBuf = sineBuffer 48000 Layout5_1 48000
        cfg = defaultSpatialMp4Config
            { smp4CoverImage = Just hdPng
            , smp4VideoFps = 160
            , smp4Metadata = emptyAudioMetadata { metaTitle = Just "Perf 160f" }
            }
    fp <- makeTempPath ".mp4"
    finally
        (do
            t0 <- fromIntegral <$> getMonotonicTimeNSec
            encodeSpatialMp4 fp cfg binBuf surBuf
            encMs <- elapsedMsSince t0
            putStrLn ("  160-frame HD (1282x722) Spatial MP4 encode: " ++ show (round encMs :: Int) ++ " ms")
            when (encMs > 2000.0) $
                fail ("160-frame HD Spatial MP4 encode exceeded 2000 ms budget: " ++ show encMs ++ " ms")
            assertMoovBeforeMdat fp "160-frame HD Spatial MP4"
            assertFfmpegDecodeClean fp "160-frame HD Spatial MP4")
        (removeFile fp)

testRepeatedEncodeMemoryStability :: IO ()
testRepeatedEncodeMemoryStability = do
    jpgBytes <- generateTestImage ".jpg" "320x240" "yuvj420p"
    let binBuf = sineBuffer 48000 LayoutStereo 24000
        surBuf = sineBuffer 48000 Layout5_1 24000
        foaBuf = sineBuffer 48000 (LayoutAmbisonic 1) 24000
        foaElem = IamfElement "FOA" IamfSceneElement foaBuf 0.0 IamfHeadphonesBinaural
        meta = AudioMetadata
            { metaTitle = Just "Loop Test"
            , metaArtist = Just "Antigravity"
            , metaAlbum = Just "Stability"
            , metaSyncedLyrics = [TimedLyric 0 "Line A", TimedLyric 250 "Line B"]
            , metaUnsyncedLyrics = Just "Line A\nLine B"
            }
        spCfg = defaultSpatialMp4Config { smp4CoverImage = Just jpgBytes, smp4Metadata = meta }
        ecCfg = defaultEclipsaMp4Config { emp4CoverImage = Just jpgBytes, emp4Metadata = meta }
        m4aCfg = defaultSurroundM4aConfig { sm4aMetadata = meta }
        runOne = do
            b1 <- encodeSpatialMp4BS spCfg binBuf surBuf
            b2 <- encodeEclipsaMp4BS ecCfg [foaElem]
            b3 <- encodeSurroundM4aBS m4aCfg surBuf
            b4 <- encodeAudioWithMetadataBS defaultMp3Config meta binBuf
            when (BS.null b1 || BS.null b2 || BS.null b3 || BS.null b4) $
                fail "Empty output during repeated encode stability check"
    runOne
    runOne
    performGC
    rssBefore <- readVmRssKb
    t0 <- fromIntegral <$> getMonotonicTimeNSec
    forM_ ([1 .. 10] :: [Int]) (const runOne)
    elapsedMs <- elapsedMsSince t0
    performGC
    rssAfter <- readVmRssKb
    let deltaKb = rssAfter - rssBefore
    putStrLn ("  40 repeated encodes (10x Spatial+Eclipsa+M4A+MP3): "
        ++ show (round elapsedMs :: Int) ++ " ms, VmRSS "
        ++ show rssBefore ++ " KB -> " ++ show rssAfter ++ " KB (delta " ++ show deltaKb ++ " KB)")
    when (deltaKb > 32768) $
        fail ("Excessive VmRSS growth across repeated encodes: " ++ show deltaKb ++ " KB")
