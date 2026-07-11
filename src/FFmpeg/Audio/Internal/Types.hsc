module FFmpeg.Audio.Internal.Types where

import Foreign.C.Types
import Data.Word (Word64)

-- | Opaque FFmpeg types (forward declarations)
data AVFormatContext
data AVIOContext
data AVCodec
data AVCodecContext
data AVFrame
data AVPacket
data AVStream
data AVChannelLayout
data AVCodecParameters

type AVSampleFormat = CInt
type AVCodecID = CInt
type AVMediaType = CInt

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/avutil.h>
#include <libavutil/error.h>
#include <libavutil/frame.h>

-- AVSampleFormat enum values
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

-- AVCodecID enum
#{enum AVCodecID,
 , avCodecIdNone           = AV_CODEC_ID_NONE
 , avCodecIdMp3            = AV_CODEC_ID_MP3
 }

-- AVMediaType enum
#{enum AVMediaType,
 , avMediaTypeAudio       = AVMEDIA_TYPE_AUDIO
 }

-- Channel layout constants
avChannelLayoutMono :: #{type uint64_t}
avChannelLayoutMono = #{const AV_CH_LAYOUT_MONO}

avChannelLayoutStereo :: #{type uint64_t}
avChannelLayoutStereo = #{const AV_CH_LAYOUT_STEREO}

-- AVFrame field offsets
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

-- | AVFrame flag: 0 means do not enforce alignment (use default)
avFrameBufferAlign0 :: CInt
avFrameBufferAlign0 = 0

-- AVIOContext flag constants
avioFlagWrite :: CInt
avioFlagWrite = #{const AVIO_FLAG_WRITE}

-- AVPacket field offsets
packetDataOffset :: Int
packetDataOffset = #{offset AVPacket, data}

packetSizeOffset :: Int
packetSizeOffset = #{offset AVPacket, size}

packetStreamIndexOffset :: Int
packetStreamIndexOffset = #{offset AVPacket, stream_index}

-- AVStream field offsets
streamCodecparOffset :: Int
streamCodecparOffset = #{offset AVStream, codecpar}

-- AVFormatContext field offsets
formatPbOffset :: Int
formatPbOffset = #{offset AVFormatContext, pb}

-- AVERROR constants for encoder loop
averrorEagain :: CInt
averrorEagain = #{const AVERROR(EAGAIN)}

averrorEof :: CInt
averrorEof = #{const AVERROR_EOF}

-- AVFrame.data array offset (first field, typically 0)
frameDataArrayOffset :: Int
frameDataArrayOffset = #{offset AVFrame, data}