-- |
-- Module      : FFmpeg.Audio
-- Description : Minimal FFmpeg 8 binding for MP3 audio encoding
-- License     : LGPL-2.1-or-later
--
-- Provides 'encodeMp3' and 'encodeMp3LBS' to encode signed 16-bit
-- interleaved PCM audio into MP3 format using FFmpeg 8 / libmp3lame.
module FFmpeg.Audio
    ( module FFmpeg.Audio.PCMBuffer
    , encodeMp3
    , encodeMp3LBS
    ) where

import FFmpeg.Audio.PCMBuffer
import FFmpeg.Audio.Encoder (encodeMp3, encodeMp3LBS)
