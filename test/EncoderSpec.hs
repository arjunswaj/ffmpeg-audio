module EncoderSpec (tests) where

import FFmpeg.Audio.Internal.Encoder (encodePcmToFile)
import FFmpeg.Audio.PCMBuffer (PCMBuffer(..))
import Data.Vector qualified as V
import Data.Int (Int16)
import System.IO (openTempFile, hClose, IOMode(ReadMode), withFile, hFileSize)
import System.Directory (doesFileExist, removeFile)

tests :: IO ()
tests = do
    testEncodeMono
    testEncodeStereo
    testEncodePartialFrame
    testEncodeEmpty
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

testEncodeMono :: IO ()
testEncodeMono = do
    fp <- makeTempPath
    let samples = V.replicate (1152 * 2) (0 :: Int16)
        buf = PCMBuffer 44100 1 samples
    encodePcmToFile fp buf
    _ <- checkFile fp "encodePcmToFile mono"
    removeFile fp

testEncodeStereo :: IO ()
testEncodeStereo = do
    fp <- makeTempPath
    let samples = V.replicate (1152 * 2 * 2) (0 :: Int16)
        buf = PCMBuffer 44100 2 samples
    encodePcmToFile fp buf
    _ <- checkFile fp "encodePcmToFile stereo"
    removeFile fp

testEncodePartialFrame :: IO ()
testEncodePartialFrame = do
    fp <- makeTempPath
    -- 1.5 frames worth of samples (1152 * 2 + 576 * 2 = 3456 interleaved stereo samples)
    let samples = V.replicate 3456 (0 :: Int16)
        buf = PCMBuffer 44100 2 samples
    encodePcmToFile fp buf
    _ <- checkFile fp "encodePcmToFile partial frame"
    removeFile fp

testEncodeEmpty :: IO ()
testEncodeEmpty = do
    fp <- makeTempPath
    let buf = PCMBuffer 44100 2 V.empty
    encodePcmToFile fp buf
    exists <- doesFileExist fp
    size <- withFile fp ReadMode hFileSize
    putStrLn $ "  encodePcmToFile empty buffer succeeded, exists: " ++ show exists ++ ", size: " ++ show size
    removeFile fp
