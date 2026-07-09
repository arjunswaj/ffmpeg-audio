-- |
-- Module      : FFmpeg.Audio.Encoder
-- Description : High-level MP3 encoding functions
-- License     : LGPL-2.1-or-later
--
-- Provides 'encodeMp3' and 'encodeMp3LBS' as the public encoding API.
module FFmpeg.Audio.Encoder
    ( encodeMp3
    , encodeMp3LBS
    ) where

import FFmpeg.Audio.Internal.Encoder (encodePcmToFile)
import FFmpeg.Audio.PCMBuffer (PCMBuffer)
import qualified Data.ByteString as BS
import System.IO (openTempFile, hClose)
import System.Directory (removeFile)

-- | Encode PCM audio data to an MP3 file.
--
-- Writes the encoded MP3 data to the given file path.
-- Throws 'FFmpegError' on failure (e.g. invalid parameters, FFmpeg error).
encodeMp3 :: FilePath -> PCMBuffer -> IO ()
encodeMp3 = encodePcmToFile

-- | Encode PCM audio data to an MP3 'BS.ByteString'.
--
-- Encodes the buffer in memory and returns the MP3 data as a strict
-- 'BS.ByteString'. Uses a temporary file internally.
-- Throws 'FFmpegError' on failure.
encodeMp3LBS :: PCMBuffer -> IO BS.ByteString
encodeMp3LBS buf = do
    (path, h) <- openTempFile "/tmp" "ffmpeg-audio.mp3"
    hClose h
    encodeMp3 path buf
    bytes <- BS.readFile path
    removeFile path
    return bytes
