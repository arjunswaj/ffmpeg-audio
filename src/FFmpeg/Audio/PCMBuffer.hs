module FFmpeg.Audio.PCMBuffer (PCMBuffer(..)) where

import Data.Int (Int16)
import Data.Vector (Vector)

data PCMBuffer = PCMBuffer
  { pcmSampleRate :: !Int
  , pcmChannels   :: !Int
  , pcmSamples    :: !(Vector Int16)
  } deriving (Show, Eq)
