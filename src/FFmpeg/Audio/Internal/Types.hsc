module FFmpeg.Audio.Internal.Types where

import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.Types
import Foreign.Storable (Storable(..))

data AVFormatContext
data AVOutputFormat
data AVIOContext
data AVCodec
data AVCodecContext
data AVFrame
data AVPacket
data AVStream
data AVChannelLayout
data AVCodecParameters
data AVDictionary
data AVStreamGroup
data AVIAMFAudioElement
data AVIAMFLayer
data AVIAMFMixPresentation
data AVIAMFSubmix
data AVIAMFSubmixElement
data AVIAMFSubmixLayout
data AVIAMFParamDefinition
data AVIAMFMixGain

type AVSampleFormat = CInt
type AVPixelFormat = CInt
type AVCodecID = CInt
type AVMediaType = CInt

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/avutil.h>
#include <libavutil/channel_layout.h>
#include <libavutil/error.h>
#include <libavutil/frame.h>
#include <libavutil/iamf.h>
#include <libavutil/opt.h>
#include <libavutil/pixfmt.h>
#include <libavutil/rational.h>

data AVRational = AVRational
    { avRatNum :: !CInt
    , avRatDen :: !CInt
    } deriving (Show, Eq)

instance Storable AVRational where
    sizeOf _ = #{size AVRational}
    alignment _ = #{alignment AVRational}
    peek ptr = do
        n <- peekByteOff ptr #{offset AVRational, num}
        d <- peekByteOff ptr #{offset AVRational, den}
        return (AVRational n d)
    poke ptr (AVRational n d) = do
        pokeByteOff ptr #{offset AVRational, num} n
        pokeByteOff ptr #{offset AVRational, den} d

#{enum AVSampleFormat,
 , avSampleFormatNone      = AV_SAMPLE_FMT_NONE
 , avSampleFormatS16       = AV_SAMPLE_FMT_S16
 , avSampleFormatS16P      = AV_SAMPLE_FMT_S16P
 , avSampleFormatS32       = AV_SAMPLE_FMT_S32
 , avSampleFormatS32P      = AV_SAMPLE_FMT_S32P
 , avSampleFormatFLT       = AV_SAMPLE_FMT_FLT
 , avSampleFormatFLTP      = AV_SAMPLE_FMT_FLTP
 , avSampleFormatDBL       = AV_SAMPLE_FMT_DBL
 , avSampleFormatDBLP      = AV_SAMPLE_FMT_DBLP
 }

#{enum AVPixelFormat,
 , avPixFmtNone            = AV_PIX_FMT_NONE
 , avPixFmtYuv420p         = AV_PIX_FMT_YUV420P
 , avPixFmtRgb24           = AV_PIX_FMT_RGB24
 , avPixFmtBgr24           = AV_PIX_FMT_BGR24
 , avPixFmtYuv422p         = AV_PIX_FMT_YUV422P
 , avPixFmtYuv444p         = AV_PIX_FMT_YUV444P
 , avPixFmtGray8           = AV_PIX_FMT_GRAY8
 , avPixFmtPal8            = AV_PIX_FMT_PAL8
 , avPixFmtYuvj420p        = AV_PIX_FMT_YUVJ420P
 , avPixFmtYuvj422p        = AV_PIX_FMT_YUVJ422P
 , avPixFmtYuvj444p        = AV_PIX_FMT_YUVJ444P
 , avPixFmtRgba            = AV_PIX_FMT_RGBA
 , avPixFmtBgra            = AV_PIX_FMT_BGRA
 , avPixFmtRgb48be         = AV_PIX_FMT_RGB48BE
 , avPixFmtRgba64be        = AV_PIX_FMT_RGBA64BE
 , avPixFmtYa8             = AV_PIX_FMT_YA8
 }

#{enum AVCodecID,
 , avCodecIdNone           = AV_CODEC_ID_NONE
 , avCodecIdMp3            = AV_CODEC_ID_MP3
 , avCodecIdFlac           = AV_CODEC_ID_FLAC
 , avCodecIdOpus           = AV_CODEC_ID_OPUS
 , avCodecIdAac            = AV_CODEC_ID_AAC
 , avCodecIdEac3           = AV_CODEC_ID_EAC3
 , avCodecIdPcmS16Le       = AV_CODEC_ID_PCM_S16LE
 , avCodecIdPcmS24Le       = AV_CODEC_ID_PCM_S24LE
 , avCodecIdH264           = AV_CODEC_ID_H264
 , avCodecIdMjpeg          = AV_CODEC_ID_MJPEG
 , avCodecIdPng            = AV_CODEC_ID_PNG
 , avCodecIdMovText        = AV_CODEC_ID_MOV_TEXT
 }

#{enum AVMediaType,
 , avMediaTypeVideo        = AVMEDIA_TYPE_VIDEO
 , avMediaTypeAudio        = AVMEDIA_TYPE_AUDIO
 , avMediaTypeSubtitle     = AVMEDIA_TYPE_SUBTITLE
 }

avColRangeMpeg :: CInt
avColRangeMpeg = #{const AVCOL_RANGE_MPEG}

avColSpcBt709 :: CInt
avColSpcBt709 = #{const AVCOL_SPC_BT709}

avColPriBt709 :: CInt
avColPriBt709 = #{const AVCOL_PRI_BT709}

avColTrcBt709 :: CInt
avColTrcBt709 = #{const AVCOL_TRC_BT709}

avDispositionDefault :: CInt
avDispositionDefault = #{const AV_DISPOSITION_DEFAULT}

avPktFlagKey :: CInt
avPktFlagKey = #{const AV_PKT_FLAG_KEY}

avInputBufferPaddingSize :: Int
avInputBufferPaddingSize = #{const AV_INPUT_BUFFER_PADDING_SIZE}

avChannelLayoutMono :: #{type uint64_t}
avChannelLayoutMono = #{const AV_CH_LAYOUT_MONO}

avChannelLayoutStereo :: #{type uint64_t}
avChannelLayoutStereo = #{const AV_CH_LAYOUT_STEREO}

avChannelLayout5Point1 :: #{type uint64_t}
avChannelLayout5Point1 = #{const AV_CH_LAYOUT_5POINT1}

avChannelLayout5Point1Back :: #{type uint64_t}
avChannelLayout5Point1Back = #{const AV_CH_LAYOUT_5POINT1_BACK}

avChannelLayout7Point1 :: #{type uint64_t}
avChannelLayout7Point1 = #{const AV_CH_LAYOUT_7POINT1}

avChannelLayout7Point1Point2 :: #{type uint64_t}
avChannelLayout7Point1Point2 = #{const AV_CH_LAYOUT_7POINT1POINT2}

avChannelLayout7Point1Point4Back :: #{type uint64_t}
avChannelLayout7Point1Point4Back = #{const AV_CH_LAYOUT_7POINT1POINT4_BACK}

avChannelLayoutBinaural :: #{type uint64_t}
#ifdef AV_CH_LAYOUT_BINAURAL
avChannelLayoutBinaural = #{const AV_CH_LAYOUT_BINAURAL}
#else
avChannelLayoutBinaural = #{const AV_CH_LAYOUT_STEREO}
#endif

avCodecCapabilitiesOffset :: Int
avCodecCapabilitiesOffset = #{offset AVCodec, capabilities}

avCodecCapSmallLastFrame :: CInt
avCodecCapSmallLastFrame = #{const AV_CODEC_CAP_SMALL_LAST_FRAME}

avCodecCapVariableFrameSize :: CInt
avCodecCapVariableFrameSize = #{const AV_CODEC_CAP_VARIABLE_FRAME_SIZE}

avCodecFlagGlobalHeader :: CInt
avCodecFlagGlobalHeader = #{const AV_CODEC_FLAG_GLOBAL_HEADER}

avFmtGlobalHeader :: CInt
avFmtGlobalHeader = #{const AVFMT_GLOBALHEADER}

avOptSearchChildren :: CInt
avOptSearchChildren = #{const AV_OPT_SEARCH_CHILDREN}

avNoptsValue :: Int64
avNoptsValue = minBound

frameNbSamplesOffset :: Int
frameNbSamplesOffset = #{offset AVFrame, nb_samples}

frameSampleRateOffset :: Int
frameSampleRateOffset = #{offset AVFrame, sample_rate}

frameFormatOffset :: Int
frameFormatOffset = #{offset AVFrame, format}

frameChLayoutOffset :: Int
frameChLayoutOffset = #{offset AVFrame, ch_layout}

framePtsOffset :: Int
framePtsOffset = #{offset AVFrame, pts}

frameDataArrayOffset :: Int
frameDataArrayOffset = #{offset AVFrame, data}

frameLinesizeOffset :: Int
frameLinesizeOffset = #{offset AVFrame, linesize}

frameWidthOffset :: Int
frameWidthOffset = #{offset AVFrame, width}

frameHeightOffset :: Int
frameHeightOffset = #{offset AVFrame, height}

frameColorRangeOffset :: Int
frameColorRangeOffset = #{offset AVFrame, color_range}

frameColorPrimariesOffset :: Int
frameColorPrimariesOffset = #{offset AVFrame, color_primaries}

frameColorTrcOffset :: Int
frameColorTrcOffset = #{offset AVFrame, color_trc}

frameColorspaceOffset :: Int
frameColorspaceOffset = #{offset AVFrame, colorspace}

frameExtendedDataOffset :: Int
frameExtendedDataOffset = #{offset AVFrame, extended_data}

avFrameBufferAlign0 :: CInt
avFrameBufferAlign0 = 0

avioFlagWrite :: CInt
avioFlagWrite = #{const AVIO_FLAG_WRITE}

packetDataOffset :: Int
packetDataOffset = #{offset AVPacket, data}

packetSizeOffset :: Int
packetSizeOffset = #{offset AVPacket, size}

packetPtsOffset :: Int
packetPtsOffset = #{offset AVPacket, pts}

packetDtsOffset :: Int
packetDtsOffset = #{offset AVPacket, dts}

packetDurationOffset :: Int
packetDurationOffset = #{offset AVPacket, duration}

packetStreamIndexOffset :: Int
packetStreamIndexOffset = #{offset AVPacket, stream_index}

packetFlagsOffset :: Int
packetFlagsOffset = #{offset AVPacket, flags}

streamIndexOffset :: Int
streamIndexOffset = #{offset AVStream, index}

streamIdOffset :: Int
streamIdOffset = #{offset AVStream, id}

streamTimeBaseOffset :: Int
streamTimeBaseOffset = #{offset AVStream, time_base}

streamCodecparOffset :: Int
streamCodecparOffset = #{offset AVStream, codecpar}

streamDispositionOffset :: Int
streamDispositionOffset = #{offset AVStream, disposition}

streamMetadataOffset :: Int
streamMetadataOffset = #{offset AVStream, metadata}

streamAvgFrameRateOffset :: Int
streamAvgFrameRateOffset = #{offset AVStream, avg_frame_rate}

codecparCodecTypeOffset :: Int
codecparCodecTypeOffset = #{offset AVCodecParameters, codec_type}

codecparCodecIdOffset :: Int
codecparCodecIdOffset = #{offset AVCodecParameters, codec_id}

codecparCodecTagOffset :: Int
codecparCodecTagOffset = #{offset AVCodecParameters, codec_tag}

codecparExtradataOffset :: Int
codecparExtradataOffset = #{offset AVCodecParameters, extradata}

codecparExtradataSizeOffset :: Int
codecparExtradataSizeOffset = #{offset AVCodecParameters, extradata_size}

formatPbOffset :: Int
formatPbOffset = #{offset AVFormatContext, pb}

formatOformatOffset :: Int
formatOformatOffset = #{offset AVFormatContext, oformat}

formatMetadataOffset :: Int
formatMetadataOffset = #{offset AVFormatContext, metadata}

formatStrictStdComplianceOffset :: Int
formatStrictStdComplianceOffset = #{offset AVFormatContext, strict_std_compliance}

oformatFlagsOffset :: Int
oformatFlagsOffset = #{offset AVOutputFormat, flags}

averrorEagain :: CInt
averrorEagain = #{const AVERROR(EAGAIN)}

averrorEof :: CInt
averrorEof = #{const AVERROR_EOF}
