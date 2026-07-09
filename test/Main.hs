module Main (main) where

import FFmpeg.Audio (PCMBuffer(..), encodeMp3)
import Data.Vector qualified as V
import System.Directory (removeFile)
import ErrorSpec qualified
import CodecSpec qualified
import CodecContextSpec qualified
import FrameSpec qualified
import PacketSpec qualified
import FormatSpec qualified
import EncoderSpec qualified

main :: IO ()
main = do
  let buf = PCMBuffer 44100 2 (V.fromList [0, 1, -1, 32767, -32768])
  putStrLn $ "PCMBuffer: " ++ show buf
  putStrLn $ "Sample rate: " ++ show (pcmSampleRate buf)
  putStrLn $ "Channels: " ++ show (pcmChannels buf)
  putStrLn $ "Samples length: " ++ show (V.length (pcmSamples buf))
  _ <- encodeMp3 "/tmp/public-api-test.mp3" buf
  putStrLn $ "encodeMp3 via public API wrote /tmp/public-api-test.mp3"
  removeFile "/tmp/public-api-test.mp3"
  ErrorSpec.tests
  CodecSpec.tests
  CodecContextSpec.tests
  FrameSpec.tests
  PacketSpec.tests
  FormatSpec.tests
  EncoderSpec.tests
  putStrLn "All tests passed!"
