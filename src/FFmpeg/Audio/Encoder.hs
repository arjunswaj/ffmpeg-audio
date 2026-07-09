module FFmpeg.Audio.Encoder
    ( encodeMp3
    , encodeMp3LBS
    ) where

import FFmpeg.Audio.Internal.Encoder (encodePcmToFile)
import FFmpeg.Audio.PCMBuffer (PCMBuffer)
import qualified Data.ByteString as BS
import System.IO (openTempFile, hClose)
import System.Directory (removeFile)

encodeMp3 :: FilePath -> PCMBuffer -> IO ()
encodeMp3 = encodePcmToFile

encodeMp3LBS :: PCMBuffer -> IO BS.ByteString
encodeMp3LBS buf = do
    (path, h) <- openTempFile "/tmp" "ffmpeg-audio.mp3"
    hClose h
    encodeMp3 path buf
    bytes <- BS.readFile path
    removeFile path
    return bytes
