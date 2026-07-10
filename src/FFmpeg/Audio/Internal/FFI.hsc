module FFmpeg.Audio.Internal.FFI where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
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

-- avcodec_parameters_from_context
foreign import ccall unsafe "avcodec_parameters_from_context"
    c_avcodec_parameters_from_context :: Ptr AVCodecParameters -> Ptr AVCodecContext -> IO CInt

-- avformat_alloc_output_context2
foreign import ccall unsafe "avformat_alloc_output_context2"
    c_avformat_alloc_output_context2 :: Ptr (Ptr AVFormatContext) -> Ptr () -> CString -> CString -> IO CInt

-- avio_open
foreign import ccall unsafe "avio_open"
    c_avio_open :: Ptr (Ptr AVIOContext) -> CString -> CInt -> IO CInt

-- avformat_new_stream
foreign import ccall unsafe "avformat_new_stream"
    c_avformat_new_stream :: Ptr AVFormatContext -> Ptr AVCodec -> IO (Ptr AVStream)

-- av_interleaved_write_frame
foreign import ccall unsafe "av_interleaved_write_frame"
    c_av_interleaved_write_frame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt

-- av_write_trailer
foreign import ccall unsafe "av_write_trailer"
    c_av_write_trailer :: Ptr AVFormatContext -> IO CInt

-- avio_closep
foreign import ccall unsafe "avio_closep"
    c_avio_closep :: Ptr (Ptr AVIOContext) -> IO CInt

-- avformat_write_header
foreign import ccall unsafe "avformat_write_header"
    c_avformat_write_header :: Ptr AVFormatContext -> Ptr () -> IO CInt

-- avformat_free_context
foreign import ccall unsafe "avformat_free_context"
    c_avformat_free_context :: Ptr AVFormatContext -> IO ()

-- avcodec_send_frame
foreign import ccall unsafe "avcodec_send_frame"
    c_avcodec_send_frame :: Ptr AVCodecContext -> Ptr AVFrame -> IO CInt

-- avcodec_receive_packet
foreign import ccall unsafe "avcodec_receive_packet"
    c_avcodec_receive_packet :: Ptr AVCodecContext -> Ptr AVPacket -> IO CInt

-- av_samples_get_buffer_size
foreign import ccall unsafe "av_samples_get_buffer_size"
    c_av_samples_get_buffer_size :: Ptr CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt