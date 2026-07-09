-- |
-- Module      : FFmpeg.Audio.PCMBuffer
-- Description : PCM audio buffer type
-- License     : LGPL-2.1-or-later
--
-- A 'PCMBuffer' holds signed 16-bit interleaved little-endian PCM
-- audio data, matching the output of fluid_synth_write_s16().
module FFmpeg.Audio.PCMBuffer (PCMBuffer(..)) where

import Data.Int (Int16)
import Data.Vector (Vector)

-- | Signed 16-bit interleaved PCM audio buffer.
--
-- Fields correspond to the parameters used for encoding:
-- sample rate, number of channels, and the interleaved sample data.
data PCMBuffer = PCMBuffer
  { pcmSampleRate :: !Int          -- ^ Sample rate in Hz (e.g. 44100)
  , pcmChannels   :: !Int          -- ^ Number of channels (1 = mono, 2 = stereo)
  , pcmSamples    :: !(Vector Int16) -- ^ Interleaved PCM samples
  } deriving (Show, Eq)
