module CodecContextSpec (tests) where

import FFmpeg.Audio.Internal.Codec (findMp3Encoder)
import FFmpeg.Audio.Internal.CodecContext
import Foreign.Ptr (nullPtr)

tests :: IO ()
tests = do
    testAllocateCodecContext
    testWithCodecContextStereo44100
    testWithCodecContextMono22050
    testFinalization
    putStrLn "All CodecContext tests passed!"

testAllocateCodecContext :: IO ()
testAllocateCodecContext = do
    codec <- findMp3Encoder
    h <- allocateCodecContext codec
    let ptr = getCodecContextPtr h
    if ptr == nullPtr
        then fail "allocateCodecContext returned nullPtr"
        else putStrLn "  allocateCodecContext succeeded (non-null)"

testWithCodecContextStereo44100 :: IO ()
testWithCodecContextStereo44100 = do
    codec <- findMp3Encoder
    withCodecContext codec 44100 2 $ \h -> do
        let ptr = getCodecContextPtr h
        if ptr == nullPtr
            then fail "withCodecContext stereo 44100 returned nullPtr"
            else putStrLn "  withCodecContext stereo @ 44100 succeeded"

testWithCodecContextMono22050 :: IO ()
testWithCodecContextMono22050 = do
    codec <- findMp3Encoder
    withCodecContext codec 22050 1 $ \h -> do
        let ptr = getCodecContextPtr h
        if ptr == nullPtr
            then fail "withCodecContext mono 22050 returned nullPtr"
            else putStrLn "  withCodecContext mono @ 22050 succeeded"

testFinalization :: IO ()
testFinalization = do
    codec <- findMp3Encoder
    h <- allocateCodecContext codec
    freeCodecContext h
    putStrLn "  freeCodecContext succeeded (no crash)"