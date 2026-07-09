module FrameSpec (tests) where

import FFmpeg.Audio.Internal.Frame
import FFmpeg.Audio.Internal.Types
import Foreign.Ptr (nullPtr)

tests :: IO ()
tests = do
    testAllocateFrame
    testWithFrameStereo44100
    testMultipleFrames
    testFrameWritable
    testFrameNbSamples
    testFinalization
    putStrLn "All Frame tests passed!"

testAllocateFrame :: IO ()
testAllocateFrame = do
    h <- allocateFrame
    let ptr = getFramePtr h
    if ptr == nullPtr
        then fail "allocateFrame returned nullPtr"
        else putStrLn "  allocateFrame succeeded (non-null)"

testWithFrameStereo44100 :: IO ()
testWithFrameStereo44100 = do
    withFrame 1024 44100 2 avSampleFormatS16P $ \h -> do
        let ptr = getFramePtr h
        if ptr == nullPtr
            then fail "withFrame returned nullPtr"
            else putStrLn "  withFrame stereo @ 44100 succeeded"

testMultipleFrames :: IO ()
testMultipleFrames = do
    h1 <- allocateFrame
    h2 <- allocateFrame
    h3 <- allocateFrame
    let p1 = getFramePtr h1
        p2 = getFramePtr h2
        p3 = getFramePtr h3
    if any (== nullPtr) [p1, p2, p3]
        then fail "multiple allocateFrame returned nullPtr"
        else putStrLn "  multiple allocateFrame succeeded"

testFrameWritable :: IO ()
testFrameWritable = do
    h <- allocateFrame
    setFrameParams h 1024 44100 2 avSampleFormatS16P
    getFrameBuffer h
    makeFrameWritable h
    putStrLn "  frame is writable after getFrameBuffer"

testFrameNbSamples :: IO ()
testFrameNbSamples = do
    putStrLn "  frame nb_samples check skipped (no getter on handle)"

testFinalization :: IO ()
testFinalization = do
    h <- allocateFrame
    freeFrame h
    putStrLn "  freeFrame succeeded (no crash)"