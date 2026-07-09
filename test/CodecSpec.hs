module CodecSpec (tests) where

import FFmpeg.Audio.Internal.Codec (findMp3Encoder)
import Control.Exception (try, SomeException)

tests :: IO ()
tests = do
    testFindMp3Encoder
    putStrLn "All codec tests passed!"

testFindMp3Encoder :: IO ()
testFindMp3Encoder = do
    r <- try findMp3Encoder
    case r of
        Left e -> do
            putStrLn $ "  ERROR: findMp3Encoder threw: " ++ show (e :: SomeException)
            fail "findMp3Encoder failed"
        Right _ -> do
            putStrLn $ "  findMp3Encoder succeeded"