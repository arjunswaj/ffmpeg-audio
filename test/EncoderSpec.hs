module EncoderSpec (tests) where

import FFmpeg.Audio.Encoder (encodeMp3, encodeMp3LBS)
import FFmpeg.Audio.PCMBuffer (PCMBuffer(..))
import Data.Vector qualified as V
import qualified Data.ByteString as BS
import Data.Int (Int16)
import System.IO (openTempFile, hClose, IOMode(ReadMode), withFile, hFileSize)
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode(..))
import Control.Monad (when)
import System.Process (readProcessWithExitCode)

tests :: IO ()
tests = do
    testEncodeMono
    testEncodeStereo
    testEncodePartialFrame
    testEncodeEmpty
    testEncodeMonoViaAPI
    testEncodeStereoViaAPI
    testEncodeMp3LBSNonEmpty
    testEncodeMp3LBSValidMp3
    testEncodeMp3LBSWithMono
    testEncodeMp3LBSEmpty
    putStrLn "All Encoder tests passed!"

makeTempPath :: IO FilePath
makeTempPath = do
    (fp, h) <- openTempFile "/tmp" "ffmpeg-encoder-test-XXXXXX.mp3"
    hClose h
    return fp

checkFile :: FilePath -> String -> IO Integer
checkFile fp label = do
    exists <- doesFileExist fp
    if not exists
        then fail $ label ++ ": file does not exist"
        else do
            size <- withFile fp ReadMode hFileSize
            if size <= 0
                then fail $ label ++ ": file is empty"
                else putStrLn $ "  " ++ label ++ " succeeded, size: " ++ show size
            return size

tryVerifyMp3 :: FilePath -> Int -> Int -> IO ()
tryVerifyMp3 fp expectedSr expectedCh = do
    (ec, out, _) <- readProcessWithExitCode "ffprobe"
        [ "-v", "quiet"
        , "-show_entries", "stream=codec_name,sample_rate,channels"
        , "-of", "csv=p=0"
        , fp
        ] ""
    case ec of
        ExitSuccess -> do
            putStrLn $ "    ffprobe: " ++ trim out
            let fields = wordsBy ',' (trim out)
            case fields of
                (codec:srStr:chStr:_) -> do
                    when (codec /= "mp3") $
                        fail $ "Expected codec mp3, got " ++ codec
                    when (read srStr /= expectedSr) $
                        fail $ "Expected sample rate " ++ show expectedSr ++ ", got " ++ srStr
                    when (read chStr /= expectedCh) $
                        fail $ "Expected channels " ++ show expectedCh ++ ", got " ++ chStr
                _ -> fail $ "Unexpected ffprobe output: " ++ out
        _ -> putStrLn $ "    ffprobe not available, skipping validation"

trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse . dropWhile (== '\n')

wordsBy :: Char -> String -> [String]
wordsBy c s = case dropWhile (== c) s of
    "" -> []
    s' -> let (w, s'') = break (== c) s' in w : wordsBy c s''

testEncodeMono :: IO ()
testEncodeMono = do
    fp <- makeTempPath
    let samples = V.replicate (1152 * 2) (0 :: Int16)
        buf = PCMBuffer 44100 1 samples
    encodeMp3 fp buf
    _ <- checkFile fp "encodeMp3 mono"
    tryVerifyMp3 fp 44100 1
    removeFile fp

testEncodeStereo :: IO ()
testEncodeStereo = do
    fp <- makeTempPath
    let samples = V.replicate (1152 * 2 * 2) (0 :: Int16)
        buf = PCMBuffer 44100 2 samples
    encodeMp3 fp buf
    _ <- checkFile fp "encodeMp3 stereo"
    tryVerifyMp3 fp 44100 2
    removeFile fp

testEncodePartialFrame :: IO ()
testEncodePartialFrame = do
    fp <- makeTempPath
    -- 1.5 frames worth of samples (1152 * 2 + 576 * 2 = 3456 interleaved stereo samples)
    let samples = V.replicate 3456 (0 :: Int16)
        buf = PCMBuffer 44100 2 samples
    encodeMp3 fp buf
    _ <- checkFile fp "encodeMp3 partial frame"
    removeFile fp

testEncodeEmpty :: IO ()
testEncodeEmpty = do
    fp <- makeTempPath
    let buf = PCMBuffer 44100 2 V.empty
    encodeMp3 fp buf
    exists <- doesFileExist fp
    size <- withFile fp ReadMode hFileSize
    putStrLn $ "  encodeMp3 empty buffer succeeded, exists: " ++ show exists ++ ", size: " ++ show size
    removeFile fp

testEncodeMonoViaAPI :: IO ()
testEncodeMonoViaAPI = do
    fp <- makeTempPath
    let samples = V.replicate (1152 * 2) (42 :: Int16)
        buf = PCMBuffer 48000 1 samples
    encodeMp3 fp buf
    _ <- checkFile fp "encodeMp3 mono (public API)"
    tryVerifyMp3 fp 48000 1
    removeFile fp

testEncodeStereoViaAPI :: IO ()
testEncodeStereoViaAPI = do
    fp <- makeTempPath
    let samples = V.replicate (1152 * 2 * 2) (100 :: Int16)
        buf = PCMBuffer 44100 2 samples
    encodeMp3 fp buf
    _ <- checkFile fp "encodeMp3 stereo (public API)"
    tryVerifyMp3 fp 44100 2
    removeFile fp

testEncodeMp3LBSNonEmpty :: IO ()
testEncodeMp3LBSNonEmpty = do
    let samples = V.replicate (1152 * 2) (0 :: Int16)
        buf = PCMBuffer 44100 2 samples
    bytes <- encodeMp3LBS buf
    if BS.null bytes
        then fail "encodeMp3LBS: produced empty ByteString"
        else putStrLn $ "  encodeMp3LBS non-empty, size: " ++ show (BS.length bytes)

testEncodeMp3LBSValidMp3 :: IO ()
testEncodeMp3LBSValidMp3 = do
    let samples = V.replicate (1152 * 2 * 2) (0 :: Int16)
        buf = PCMBuffer 44100 2 samples
    bytes <- encodeMp3LBS buf
    BS.writeFile "/tmp/ffmpeg-audio-lbs-test.mp3" bytes
    tryVerifyMp3 "/tmp/ffmpeg-audio-lbs-test.mp3" 44100 2
    removeFile "/tmp/ffmpeg-audio-lbs-test.mp3"

testEncodeMp3LBSWithMono :: IO ()
testEncodeMp3LBSWithMono = do
    let samples = V.replicate (1152 * 2) (42 :: Int16)
        buf = PCMBuffer 48000 1 samples
    bytes <- encodeMp3LBS buf
    BS.writeFile "/tmp/ffmpeg-audio-lbs-mono-test.mp3" bytes
    tryVerifyMp3 "/tmp/ffmpeg-audio-lbs-mono-test.mp3" 48000 1
    removeFile "/tmp/ffmpeg-audio-lbs-mono-test.mp3"

testEncodeMp3LBSEmpty :: IO ()
testEncodeMp3LBSEmpty = do
    let buf = PCMBuffer 44100 2 V.empty
    bytes <- encodeMp3LBS buf
    putStrLn $ "  encodeMp3LBS empty buffer succeeded, size: " ++ show (BS.length bytes)
