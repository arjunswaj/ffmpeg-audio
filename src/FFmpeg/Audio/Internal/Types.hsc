module FFmpeg.Audio.Internal.Types where

import Foreign.C.Types
import Foreign.Ptr
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