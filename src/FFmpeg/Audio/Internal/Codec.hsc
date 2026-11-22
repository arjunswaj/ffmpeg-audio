module FFmpeg.Audio.Internal.Codec
    ( findMp3Encoder
    , findEncoderByName
    , findEncoderById
    , findDecoderById
    , findDecoderByName
    , CodecHandle(..)
    , getCodecPtr
    , codecCapabilities
    , codecSupportsSmallLastFrame
    ) where

import Control.Exception (throwIO)
import Data.Bits ((.&.), (.|.))
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekByteOff)
import FFmpeg.Audio.Internal.Types
    ( AVCodec
    , AVCodecID
    , avCodecCapabilitiesOffset
    , avCodecCapSmallLastFrame
    , avCodecCapVariableFrameSize
    )
import FFmpeg.Audio.Internal.FFI
    ( c_avcodec_find_decoder
    , c_avcodec_find_decoder_by_name
    , c_avcodec_find_encoder
    , c_avcodec_find_encoder_by_name
    )
import FFmpeg.Audio.Internal.Error (FFmpegError(..))

#include <libavcodec/avcodec.h>

newtype CodecHandle = CodecHandle (Ptr AVCodec)

findMp3Encoder :: IO CodecHandle
findMp3Encoder =
    withCString "libmp3lame" $ \name -> do
        ptr <- c_avcodec_find_encoder_by_name name
        if ptr == nullPtr
            then throwIO $ FFmpegError (-1) "MP3 encoder (libmp3lame) not found. Install FFmpeg with --enable-libmp3lame."
            else return $ CodecHandle ptr

findEncoderByName :: String -> IO CodecHandle
findEncoderByName codecName =
    withCString codecName $ \cName -> do
        ptr <- c_avcodec_find_encoder_by_name cName
        if ptr == nullPtr
            then throwIO $ FFmpegError (-1) ("Audio encoder not found: " ++ codecName)
            else return $ CodecHandle ptr

findEncoderById :: AVCodecID -> IO CodecHandle
findEncoderById codecId = do
    ptr <- c_avcodec_find_encoder codecId
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) ("Audio encoder not found for codec id: " ++ show codecId)
        else return $ CodecHandle ptr

findDecoderById :: AVCodecID -> IO CodecHandle
findDecoderById codecId = do
    ptr <- c_avcodec_find_decoder codecId
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) ("Decoder not found for codec id: " ++ show codecId)
        else return $ CodecHandle ptr

findDecoderByName :: String -> IO CodecHandle
findDecoderByName codecName =
    withCString codecName $ \cName -> do
        ptr <- c_avcodec_find_decoder_by_name cName
        if ptr == nullPtr
            then throwIO $ FFmpegError (-1) ("Decoder not found: " ++ codecName)
            else return $ CodecHandle ptr

getCodecPtr :: CodecHandle -> Ptr AVCodec
getCodecPtr (CodecHandle p) = p

codecCapabilities :: CodecHandle -> IO CInt
codecCapabilities (CodecHandle p) =
    peekByteOff (castPtr p) avCodecCapabilitiesOffset

codecSupportsSmallLastFrame :: CodecHandle -> IO Bool
codecSupportsSmallLastFrame ch = do
    caps <- codecCapabilities ch
    return ((caps .&. (avCodecCapSmallLastFrame .|. avCodecCapVariableFrameSize)) /= 0)
