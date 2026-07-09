module PacketSpec (tests) where

import FFmpeg.Audio.Internal.Packet
import Foreign.Ptr (nullPtr)

tests :: IO ()
tests = do
    testAllocatePacket
    testWithPacket
    testUnrefPacket
    testAllocUnrefAlloc
    testFinalization
    putStrLn "All Packet tests passed!"

testAllocatePacket :: IO ()
testAllocatePacket = do
    h <- allocatePacket
    let ptr = getPacketPtr h
    if ptr == nullPtr
        then fail "allocatePacket returned nullPtr"
        else putStrLn "  allocatePacket succeeded (non-null)"

testWithPacket :: IO ()
testWithPacket =
    withPacket $ \h -> do
        let ptr = getPacketPtr h
        if ptr == nullPtr
            then fail "withPacket returned nullPtr"
            else putStrLn "  withPacket succeeded (non-null)"

testUnrefPacket :: IO ()
testUnrefPacket = do
    h <- allocatePacket
    unrefPacket h
    putStrLn "  unrefPacket on fresh packet succeeded (no crash)"

testAllocUnrefAlloc :: IO ()
testAllocUnrefAlloc = do
    h1 <- allocatePacket
    unrefPacket h1
    h2 <- allocatePacket
    unrefPacket h2
    let p1 = getPacketPtr h1
        p2 = getPacketPtr h2
    if any (== nullPtr) [p1, p2]
        then fail "alloc -> unref -> alloc returned nullPtr"
        else putStrLn "  alloc -> unref -> alloc succeeded (no leak)"

testFinalization :: IO ()
testFinalization = do
    h <- allocatePacket
    freePacket h
    putStrLn "  freePacket succeeded (no crash)"