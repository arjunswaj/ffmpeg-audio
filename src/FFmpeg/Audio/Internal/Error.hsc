{-# LANGUAGE ForeignFunctionInterface #-}

module FFmpeg.Audio.Internal.Error
    ( FFmpegError(..)
    , throwFFmpegError
    , avErrorToString
    , errorNonNegative
    ) where

import Control.Exception (Exception, throwIO)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CChar(..), CInt(..), CULong(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/avutil.h>
#include <libavutil/error.h>

foreign import ccall unsafe "av_strerror"
    c_av_strerror :: CInt -> Ptr CChar -> CULong -> IO CInt

data FFmpegError = FFmpegError
    { ffErrorCode   :: !Int
    , ffErrorMessage :: !String
    } deriving (Show, Eq)

instance Exception FFmpegError

avErrorToString :: Int -> IO String
avErrorToString errCode =
    allocaBytes 64 $ \buf -> do
        _ <- c_av_strerror (fromIntegral errCode) buf (64 :: CULong)
        peekCString buf

throwFFmpegError :: Int -> IO a
throwFFmpegError code = do
    msg <- avErrorToString code
    throwIO $ FFmpegError code msg

errorNonNegative :: String -> Int -> IO Int
errorNonNegative context code
    | code < 0   = do
        msg <- avErrorToString code
        throwIO $ FFmpegError code (context ++ ": " ++ msg)
    | otherwise  = return code