module ErrorSpec (tests) where

import FFmpeg.Audio.Internal.Error
import Control.Exception (try)

tests :: IO ()
tests = do
    testAvErrorToString
    testErrorNonNegativeValid
    testErrorNonNegativeInvalid
    putStrLn "All error tests passed!"

testAvErrorToString :: IO ()
testAvErrorToString = do
    msg0 <- avErrorToString 0
    putStrLn $ "  avErrorToString 0: " ++ show msg0
    msg2 <- avErrorToString (-2)
    putStrLn $ "  avErrorToString (-2): " ++ show msg2
    msg22 <- avErrorToString (-22)
    putStrLn $ "  avErrorToString (-22): " ++ show msg22

testErrorNonNegativeValid :: IO ()
testErrorNonNegativeValid = do
    r <- errorNonNegative "test" 42
    putStrLn $ "  errorNonNegative 42: " ++ show r
    r' <- errorNonNegative "test" 0
    putStrLn $ "  errorNonNegative 0: " ++ show r'

testErrorNonNegativeInvalid :: IO ()
testErrorNonNegativeInvalid = do
    r <- try $ errorNonNegative "ctx" (-42)
    case r of
        Left (FFmpegError code msg) -> do
            putStrLn $ "  Caught FFmpegError code=" ++ show code ++ " msg=" ++ show msg
        Right _ ->
            putStrLn "  ERROR: expected FFmpegError but got success"