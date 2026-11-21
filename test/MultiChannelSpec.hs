module MultiChannelSpec (tests) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Vector.Storable qualified as VS
import FFmpeg.Audio.Encoder
import FFmpeg.Audio.PCMBuffer
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import System.IO (IOMode(ReadMode), hClose, hFileSize, openTempFile, withFile)
import System.Process (readProcessWithExitCode)

tests :: IO ()
tests = do
    testFloatMp3Stereo48k
    testFlac24Bit5_1
    testFlac24Bit7_1
    testPcm24Bit7_1_4
    testOpusFoaAmbisonicsFamily2
    testOpusHoa3AmbisonicsFamily2
    testAacStereo48k
    testEac3Surround5_1
    testEncodeAudioBSFlac
    putStrLn "All MultiChannel tests passed!"

makeTempPath :: String -> IO FilePath
makeTempPath ext = do
    (fp, h) <- openTempFile "/tmp" ("ffmpeg-mc-test" ++ ext)
    hClose h
    return fp

sineFloat :: Int -> Int -> Int -> VS.Vector Float
sineFloat sr ch frames =
    VS.generate (frames * ch) $ \idx ->
        let f = idx `div` ch
            c = idx `mod` ch
            freq = 220.0 + fromIntegral c * 55.0 :: Double
            t = fromIntegral f / fromIntegral sr :: Double
        in realToFrac (0.25 * sin (2.0 * pi * freq * t))

sineS32 :: Int -> Int -> Int -> VS.Vector Int32
sineS32 sr ch frames =
    VS.map toS32 (sineFloat sr ch frames)
  where
    toS32 x = fromIntegral (round (x * 8388607.0) :: Int) * 256

assertNonEmpty :: FilePath -> String -> IO ()
assertNonEmpty fp label = do
    exists <- doesFileExist fp
    unless exists $ fail (label ++ ": file does not exist")
    sz <- withFile fp ReadMode hFileSize
    when (sz <= 0) $ fail (label ++ ": file is empty")
    putStrLn ("  " ++ label ++ " succeeded, size: " ++ show sz)

ffprobeStream :: FilePath -> String -> IO String
ffprobeStream fp entries = do
    (ec, out, err) <- readProcessWithExitCode "ffprobe"
        [ "-v", "error"
        , "-show_entries", entries
        , "-of", "csv=p=0"
        , fp
        ] ""
    case ec of
        ExitSuccess -> return (trim out)
        ExitFailure code -> fail ("ffprobe failed (" ++ show code ++ "): " ++ err)

trim :: String -> String
trim = reverse . dropWhile (`elem` ("\r\n " :: String)) . reverse . dropWhile (`elem` ("\r\n " :: String))

wordsBy :: Char -> String -> [String]
wordsBy c s = case dropWhile (== c) s of
    "" -> []
    s' -> let (w, s'') = break (== c) s' in w : wordsBy c s''

testFloatMp3Stereo48k :: IO ()
testFloatMp3Stereo48k = do
    fp <- makeTempPath ".mp3"
    let buf = PCMBufferFloat 48000 LayoutStereo (sineFloat 48000 2 9600)
    finally
        (do
            encodeFloatMp3 fp buf
            assertNonEmpty fp "encodeFloatMp3 stereo 48kHz"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("mp3" : "48000" : "2" : _) -> return ()
                _ -> fail ("Unexpected ffprobe output for float MP3: " ++ out))
        (removeFile fp)

testFlac24Bit5_1 :: IO ()
testFlac24Bit5_1 = do
    fp <- makeTempPath ".flac"
    let buf = AudioBuffer 48000 Layout5_1 (SampleDataS32 (sineS32 48000 6 12000))
    finally
        (do
            encodeFlac24 fp buf
            assertNonEmpty fp "encodeFlac24 5.1 48kHz 24-bit"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels,bits_per_raw_sample"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("flac" : "48000" : "6" : "24" : _) -> return ()
                _ -> fail ("Unexpected ffprobe output for 5.1 FLAC: " ++ out))
        (removeFile fp)

testFlac24Bit7_1 :: IO ()
testFlac24Bit7_1 = do
    fp <- makeTempPath ".flac"
    let buf = AudioBuffer 48000 Layout7_1 (SampleDataFloat (sineFloat 48000 8 12000))
    finally
        (do
            encodeFlac24 fp buf
            assertNonEmpty fp "encodeFlac24 7.1 48kHz 24-bit"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels,bits_per_raw_sample"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("flac" : "48000" : "8" : "24" : _) -> return ()
                _ -> fail ("Unexpected ffprobe output for 7.1 FLAC: " ++ out))
        (removeFile fp)

testPcm24Bit7_1_4 :: IO ()
testPcm24Bit7_1_4 = do
    fp <- makeTempPath ".wav"
    let buf = AudioBuffer 48000 Layout7_1_4 (SampleDataFloat (sineFloat 48000 12 4800))
        cfg = CodecConfig
            { codecType = CodecPcmS24Le
            , codecBitRate = Nothing
            , codecBitDepth = Just 24
            , codecFormatName = Just "wav"
            , codecOpusMappingFamily = Nothing
            }
    finally
        (do
            encodeAudio fp cfg buf
            assertNonEmpty fp "encodeAudio 7.1.4 12ch 48kHz 24-bit PCM"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels,bits_per_sample"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("pcm_s24le" : "48000" : "12" : "24" : _) -> return ()
                _ -> fail ("Unexpected ffprobe output for 7.1.4 24-bit WAV: " ++ out))
        (removeFile fp)

testOpusFoaAmbisonicsFamily2 :: IO ()
testOpusFoaAmbisonicsFamily2 = do
    fp <- makeTempPath ".opus"
    let buf = AudioBuffer 48000 (LayoutAmbisonic 1) (SampleDataFloat (sineFloat 48000 4 9600))
        cfg = defaultOpusAmbisonicsConfig 2
    finally
        (do
            encodeOpus fp cfg buf
            assertNonEmpty fp "encodeOpus FOA Ambisonics (family 2)"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels,channel_layout"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("opus" : "48000" : "4" : layoutStr : _) ->
                    unless ("ambisonic" `isInfixOf` layoutStr) $
                        fail ("Expected ambisonic channel layout, got: " ++ layoutStr)
                _ -> fail ("Unexpected ffprobe output for FOA Opus (family 2): " ++ out))
        (removeFile fp)

testOpusHoa3AmbisonicsFamily2 :: IO ()
testOpusHoa3AmbisonicsFamily2 = do
    fp <- makeTempPath ".opus"
    let buf = AudioBuffer 48000 (LayoutAmbisonic 3) (SampleDataFloat (sineFloat 48000 16 4800))
        cfg = (defaultOpusAmbisonicsConfig 2) { codecBitRate = Just 512000 }
    finally
        (do
            encodeOpus fp cfg buf
            assertNonEmpty fp "encodeOpus HOA3 16ch Ambisonics (family 2)"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels,channel_layout"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("opus" : "48000" : "16" : layoutStr : _) ->
                    unless ("ambisonic" `isInfixOf` layoutStr) $
                        fail ("Expected ambisonic channel layout, got: " ++ layoutStr)
                _ -> fail ("Unexpected ffprobe output for HOA3 Opus (family 2): " ++ out))
        (removeFile fp)

testAacStereo48k :: IO ()
testAacStereo48k = do
    fp <- makeTempPath ".m4a"
    let buf = AudioBuffer 48000 LayoutStereo (SampleDataFloat (sineFloat 48000 2 9600))
        cfg = defaultAacConfig { codecFormatName = Just "mp4" }
    finally
        (do
            encodeAudio fp cfg buf
            assertNonEmpty fp "encodeAudio AAC stereo 48kHz"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("aac" : "48000" : "2" : _) -> return ()
                _ -> fail ("Unexpected ffprobe output for AAC: " ++ out))
        (removeFile fp)

testEac3Surround5_1 :: IO ()
testEac3Surround5_1 = do
    fp <- makeTempPath ".eac3"
    let buf = AudioBuffer 48000 Layout5_1 (SampleDataFloat (sineFloat 48000 6 9600))
    finally
        (do
            encodeAudio fp defaultEac3Config buf
            assertNonEmpty fp "encodeAudio E-AC-3 5.1 48kHz"
            out <- ffprobeStream fp "stream=codec_name,sample_rate,channels"
            putStrLn ("    ffprobe: " ++ out)
            case wordsBy ',' out of
                ("eac3" : "48000" : "6" : _) -> return ()
                _ -> fail ("Unexpected ffprobe output for E-AC-3: " ++ out))
        (removeFile fp)

testEncodeAudioBSFlac :: IO ()
testEncodeAudioBSFlac = do
    let buf = AudioBuffer 48000 LayoutStereo (SampleDataFloat (sineFloat 48000 2 4800))
    bytes <- encodeAudioBS defaultFlac24Config buf
    when (BS.null bytes) $ fail "encodeAudioBS FLAC produced empty ByteString"
    when (BS.take 4 bytes /= BS.pack [0x66, 0x4c, 0x61, 0x43]) $
        fail "encodeAudioBS FLAC missing fLaC magic header"
    putStrLn ("  encodeAudioBS FLAC 24-bit succeeded, size: " ++ show (BS.length bytes))
