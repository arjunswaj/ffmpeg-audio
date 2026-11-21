module FFmpeg.Audio.PCMBuffer
    ( PCMBuffer(..)
    , ChannelLayout(..)
    , channelLayoutCount
    , SampleData(..)
    , PCMBufferFloat(..)
    , AudioBuffer(..)
    , fromPCMBuffer
    , fromPCMBufferFloat
    , audioChannels
    , audioFrameCount
    ) where

import Data.Int (Int16, Int32)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS

data PCMBuffer = PCMBuffer
    { pcmSampleRate :: !Int
    , pcmChannels   :: !Int
    , pcmSamples    :: !(Vector Int16)
    } deriving (Show, Eq)

data ChannelLayout
    = LayoutMono
    | LayoutStereo
    | Layout5_1
    | Layout7_1
    | Layout7_1_2
    | Layout7_1_4
    | LayoutAmbisonic !Int
    | LayoutBinaural
    | LayoutDefault !Int
    deriving (Show, Eq)

channelLayoutCount :: ChannelLayout -> Int
channelLayoutCount LayoutMono = 1
channelLayoutCount LayoutStereo = 2
channelLayoutCount Layout5_1 = 6
channelLayoutCount Layout7_1 = 8
channelLayoutCount Layout7_1_2 = 10
channelLayoutCount Layout7_1_4 = 12
channelLayoutCount (LayoutAmbisonic order) = (order + 1) * (order + 1)
channelLayoutCount LayoutBinaural = 2
channelLayoutCount (LayoutDefault n) = n

data SampleData
    = SampleDataS16 !(VS.Vector Int16)
    | SampleDataS32 !(VS.Vector Int32)
    | SampleDataFloat !(VS.Vector Float)
    deriving (Show, Eq)

data PCMBufferFloat = PCMBufferFloat
    { pcmFloatSampleRate :: !Int
    , pcmFloatLayout     :: !ChannelLayout
    , pcmFloatSamples    :: !(VS.Vector Float)
    } deriving (Show, Eq)

data AudioBuffer = AudioBuffer
    { audioSampleRate :: !Int
    , audioLayout     :: !ChannelLayout
    , audioSamples    :: !SampleData
    } deriving (Show, Eq)

fromPCMBuffer :: PCMBuffer -> AudioBuffer
fromPCMBuffer buf =
    let layout = case pcmChannels buf of
            1 -> LayoutMono
            2 -> LayoutStereo
            n -> LayoutDefault n
        vec = VS.generate (V.length (pcmSamples buf)) (V.unsafeIndex (pcmSamples buf))
    in AudioBuffer
        { audioSampleRate = pcmSampleRate buf
        , audioLayout = layout
        , audioSamples = SampleDataS16 vec
        }

fromPCMBufferFloat :: PCMBufferFloat -> AudioBuffer
fromPCMBufferFloat buf =
    AudioBuffer
        { audioSampleRate = pcmFloatSampleRate buf
        , audioLayout = pcmFloatLayout buf
        , audioSamples = SampleDataFloat (pcmFloatSamples buf)
        }

audioChannels :: AudioBuffer -> Int
audioChannels = channelLayoutCount . audioLayout

audioFrameCount :: AudioBuffer -> Int
audioFrameCount buf =
    let ch = audioChannels buf
        totalLen = case audioSamples buf of
            SampleDataS16 v -> VS.length v
            SampleDataS32 v -> VS.length v
            SampleDataFloat v -> VS.length v
    in if ch <= 0 then 0 else totalLen `div` ch
