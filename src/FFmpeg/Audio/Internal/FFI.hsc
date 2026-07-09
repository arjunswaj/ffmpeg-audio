module FFmpeg.Audio.Internal.FFI where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable (Storable(..))
import FFmpeg.Audio.Internal.Types

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/avutil.h>
#include <libavutil/error.h>
#include <libavutil/frame.h>

-- av_strerror
foreign import ccall unsafe "av_strerror"
    c_av_strerror :: CInt -> Ptr CChar -> CULong -> CInt

-- avcodec_find_encoder_by_name
foreign import ccall unsafe "avcodec_find_encoder_by_name"
    c_avcodec_find_encoder_by_name :: CString -> IO (Ptr AVCodec)

foreign import ccall unsafe "avcodec_alloc_context3"
    c_avcodec_alloc_context3 :: Ptr AVCodec -> IO (Ptr AVCodecContext)

foreign import ccall unsafe "avcodec_open2"
    c_avcodec_open2 :: Ptr AVCodecContext -> Ptr AVCodec -> Ptr () -> IO CInt

foreign import ccall unsafe "avcodec_free_context"
    c_avcodec_free_context :: Ptr (Ptr AVCodecContext) -> IO ()

foreign import ccall unsafe "av_channel_layout_default"
    av_channel_layout_default :: Ptr AVChannelLayout -> CInt -> IO ()

foreign import ccall unsafe "av_frame_alloc"
    c_av_frame_alloc :: IO (Ptr AVFrame)

foreign import ccall unsafe "av_frame_get_buffer"
    c_av_frame_get_buffer :: Ptr AVFrame -> CInt -> IO CInt

foreign import ccall unsafe "av_frame_make_writable"
    c_av_frame_make_writable :: Ptr AVFrame -> IO CInt

foreign import ccall unsafe "av_frame_free"
    c_av_frame_free :: Ptr (Ptr AVFrame) -> IO ()

-- av_packet_alloc
foreign import ccall unsafe "av_packet_alloc"
    c_av_packet_alloc :: IO (Ptr AVPacket)

-- av_packet_unref
foreign import ccall unsafe "av_packet_unref"
    c_av_packet_unref :: Ptr AVPacket -> IO ()

-- av_packet_free
foreign import ccall unsafe "av_packet_free"
    c_av_packet_free :: Ptr (Ptr AVPacket) -> IO ()