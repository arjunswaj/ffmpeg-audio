module FFmpeg.Audio.Internal.Codec
    ( findMp3Encoder
    , CodecHandle(..)
    ) where

import Control.Exception (throwIO)
import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr, nullPtr)
import FFmpeg.Audio.Internal.Types (AVCodec)
import FFmpeg.Audio.Internal.FFI (c_avcodec_find_encoder_by_name)
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