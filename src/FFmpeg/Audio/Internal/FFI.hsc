module FFmpeg.Audio.Internal.FFI where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import FFmpeg.Audio.Internal.Types

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/avutil.h>
#include <libavutil/channel_layout.h>
#include <libavutil/dict.h>
#include <libavutil/error.h>
#include <libavutil/frame.h>
#include <libavutil/iamf.h>
#include <libavutil/mem.h>
#include <libavutil/opt.h>
#include <libavutil/samplefmt.h>

foreign import ccall unsafe "av_strerror"
    c_av_strerror :: CInt -> Ptr CChar -> CULong -> CInt

foreign import ccall unsafe "avcodec_find_encoder_by_name"
    c_avcodec_find_encoder_by_name :: CString -> IO (Ptr AVCodec)

foreign import ccall unsafe "avcodec_find_encoder"
    c_avcodec_find_encoder :: AVCodecID -> IO (Ptr AVCodec)

foreign import ccall unsafe "avcodec_alloc_context3"
    c_avcodec_alloc_context3 :: Ptr AVCodec -> IO (Ptr AVCodecContext)

foreign import ccall unsafe "avcodec_open2"
    c_avcodec_open2 :: Ptr AVCodecContext -> Ptr AVCodec -> Ptr () -> IO CInt

foreign import ccall unsafe "avcodec_free_context"
    c_avcodec_free_context :: Ptr (Ptr AVCodecContext) -> IO ()

foreign import ccall unsafe "av_channel_layout_default"
    av_channel_layout_default :: Ptr AVChannelLayout -> CInt -> IO ()

foreign import ccall unsafe "av_channel_layout_from_mask"
    c_av_channel_layout_from_mask :: Ptr AVChannelLayout -> Word64 -> IO CInt

foreign import ccall unsafe "av_channel_layout_from_string"
    c_av_channel_layout_from_string :: Ptr AVChannelLayout -> CString -> IO CInt

foreign import ccall unsafe "av_channel_layout_copy"
    c_av_channel_layout_copy :: Ptr AVChannelLayout -> Ptr AVChannelLayout -> IO CInt

foreign import ccall unsafe "av_channel_layout_uninit"
    c_av_channel_layout_uninit :: Ptr AVChannelLayout -> IO ()

foreign import ccall unsafe "av_opt_set"
    c_av_opt_set :: Ptr () -> CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "av_opt_set_int"
    c_av_opt_set_int :: Ptr () -> CString -> Int64 -> CInt -> IO CInt

foreign import ccall unsafe "av_dict_set"
    c_av_dict_set :: Ptr (Ptr AVDictionary) -> CString -> CString -> CInt -> IO CInt

foreign import ccall unsafe "av_dict_free"
    c_av_dict_free :: Ptr (Ptr AVDictionary) -> IO ()

foreign import ccall unsafe "av_free"
    c_av_free :: Ptr () -> IO ()

foreign import ccall unsafe "av_sample_fmt_is_planar"
    c_av_sample_fmt_is_planar :: AVSampleFormat -> CInt

foreign import ccall unsafe "av_frame_alloc"
    c_av_frame_alloc :: IO (Ptr AVFrame)

foreign import ccall unsafe "av_frame_get_buffer"
    c_av_frame_get_buffer :: Ptr AVFrame -> CInt -> IO CInt

foreign import ccall unsafe "av_frame_make_writable"
    c_av_frame_make_writable :: Ptr AVFrame -> IO CInt

foreign import ccall unsafe "av_frame_free"
    c_av_frame_free :: Ptr (Ptr AVFrame) -> IO ()

foreign import ccall unsafe "av_packet_alloc"
    c_av_packet_alloc :: IO (Ptr AVPacket)

foreign import ccall unsafe "av_packet_unref"
    c_av_packet_unref :: Ptr AVPacket -> IO ()

foreign import ccall unsafe "av_packet_free"
    c_av_packet_free :: Ptr (Ptr AVPacket) -> IO ()

foreign import ccall unsafe "avcodec_parameters_from_context"
    c_avcodec_parameters_from_context :: Ptr AVCodecParameters -> Ptr AVCodecContext -> IO CInt

foreign import ccall unsafe "avformat_alloc_output_context2"
    c_avformat_alloc_output_context2 :: Ptr (Ptr AVFormatContext) -> Ptr () -> CString -> CString -> IO CInt

foreign import ccall unsafe "avio_open"
    c_avio_open :: Ptr (Ptr AVIOContext) -> CString -> CInt -> IO CInt

foreign import ccall unsafe "avformat_new_stream"
    c_avformat_new_stream :: Ptr AVFormatContext -> Ptr AVCodec -> IO (Ptr AVStream)

foreign import ccall unsafe "av_interleaved_write_frame"
    c_av_interleaved_write_frame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt

foreign import ccall unsafe "av_write_trailer"
    c_av_write_trailer :: Ptr AVFormatContext -> IO CInt

foreign import ccall unsafe "avio_closep"
    c_avio_closep :: Ptr (Ptr AVIOContext) -> IO CInt

foreign import ccall unsafe "avformat_write_header"
    c_avformat_write_header :: Ptr AVFormatContext -> Ptr () -> IO CInt

foreign import ccall unsafe "avformat_free_context"
    c_avformat_free_context :: Ptr AVFormatContext -> IO ()

foreign import ccall unsafe "avcodec_send_frame"
    c_avcodec_send_frame :: Ptr AVCodecContext -> Ptr AVFrame -> IO CInt

foreign import ccall unsafe "avcodec_receive_packet"
    c_avcodec_receive_packet :: Ptr AVCodecContext -> Ptr AVPacket -> IO CInt

foreign import ccall unsafe "av_samples_get_buffer_size"
    c_av_samples_get_buffer_size :: Ptr CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "av_iamf_audio_element_alloc"
    c_av_iamf_audio_element_alloc :: IO (Ptr AVIAMFAudioElement)

foreign import ccall unsafe "av_iamf_audio_element_add_layer"
    c_av_iamf_audio_element_add_layer :: Ptr AVIAMFAudioElement -> IO (Ptr AVIAMFLayer)

foreign import ccall unsafe "av_iamf_audio_element_free"
    c_av_iamf_audio_element_free :: Ptr (Ptr AVIAMFAudioElement) -> IO ()

foreign import ccall unsafe "av_iamf_mix_presentation_alloc"
    c_av_iamf_mix_presentation_alloc :: IO (Ptr AVIAMFMixPresentation)

foreign import ccall unsafe "av_iamf_mix_presentation_add_submix"
    c_av_iamf_mix_presentation_add_submix :: Ptr AVIAMFMixPresentation -> IO (Ptr AVIAMFSubmix)

foreign import ccall unsafe "av_iamf_submix_add_element"
    c_av_iamf_submix_add_element :: Ptr AVIAMFSubmix -> IO (Ptr AVIAMFSubmixElement)

foreign import ccall unsafe "av_iamf_submix_add_layout"
    c_av_iamf_submix_add_layout :: Ptr AVIAMFSubmix -> IO (Ptr AVIAMFSubmixLayout)

foreign import ccall unsafe "av_iamf_mix_presentation_free"
    c_av_iamf_mix_presentation_free :: Ptr (Ptr AVIAMFMixPresentation) -> IO ()

foreign import ccall unsafe "av_iamf_param_definition_alloc"
    c_av_iamf_param_definition_alloc :: CInt -> CUInt -> Ptr CSize -> IO (Ptr AVIAMFParamDefinition)

foreign import ccall unsafe "avformat_stream_group_create"
    c_avformat_stream_group_create :: Ptr AVFormatContext -> CInt -> Ptr (Ptr AVDictionary) -> IO (Ptr AVStreamGroup)

foreign import ccall unsafe "avformat_stream_group_add_stream"
    c_avformat_stream_group_add_stream :: Ptr AVStreamGroup -> Ptr AVStream -> IO CInt
