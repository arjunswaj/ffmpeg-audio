module FormatSpec (tests) where

import Control.Exception (SomeException, catch)
import FFmpeg.Audio.Internal.Codec (findMp3Encoder)
import FFmpeg.Audio.Internal.CodecContext (withCodecContext)
import FFmpeg.Audio.Internal.Format
import System.IO (openTempFile, hClose)

tests :: IO ()
tests = do
    testAllocateOutputContext
    testCreateStreamAndSetParams
    testFullCycle
    testCleanupHandlesErrors
    putStrLn "All Format tests passed!"

makeTempPath :: IO FilePath
makeTempPath = do
    (fp, h) <- openTempFile "/tmp" "ffmpeg-test-XXXXXX.mp3"
    hClose h
    return fp

testAllocateOutputContext :: IO ()
testAllocateOutputContext = do
    fp <- makeTempPath
    h <- allocateOutputContext fp
    freeFormatContext h
    putStrLn $ "  allocateOutputContext succeeded for " ++ fp

testCreateStreamAndSetParams :: IO ()
testCreateStreamAndSetParams = do
    fp <- makeTempPath
    codec <- findMp3Encoder
    withCodecContext codec 44100 2 $ \cch -> do
        fmt <- allocateOutputContext fp
        openOutput fmt fp
        sh <- createStream fmt codec
        setCodecParameters sh cch
        closeOutput fmt
        freeFormatContext fmt
        putStrLn "  createStream + setCodecParameters succeeded"

testFullCycle :: IO ()
testFullCycle = do
    fp <- makeTempPath
    codec <- findMp3Encoder
    withCodecContext codec 44100 2 $ \cch -> do
        withFormatContext fp $ \fmt -> do
            openOutput fmt fp
            sh <- createStream fmt codec
            setCodecParameters sh cch
            writeHeader fmt
            writeTrailer fmt
            closeOutput fmt
            putStrLn "  full cycle (withFormatContext + open + stream + params + header + trailer + close) succeeded"

testCleanupHandlesErrors :: IO ()
testCleanupHandlesErrors = do
    fp <- makeTempPath
    fmt <- allocateOutputContext fp
    openOutput fmt fp `catch` (\(_ :: SomeException) -> return ())
    closeOutput fmt `catch` (\(_ :: SomeException) -> return ())
    freeFormatContext fmt
    putStrLn "  cleanup (close + free) handles correctly"