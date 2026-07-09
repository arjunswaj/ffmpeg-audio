module Main (main) where

import FFmpeg.Audio.PCMBuffer (PCMBuffer (..))
import Data.Vector qualified as V
import ErrorSpec qualified
import CodecSpec qualified
import CodecContextSpec qualified
import FrameSpec qualified
import PacketSpec qualified

main :: IO ()
main = do
  let buf = PCMBuffer 44100 2 (V.fromList [0, 1, -1, 32767, -32768])
  putStrLn $ "PCMBuffer: " ++ show buf
  putStrLn $ "Sample rate: " ++ show (pcmSampleRate buf)
  putStrLn $ "Channels: " ++ show (pcmChannels buf)
  putStrLn $ "Samples length: " ++ show (V.length (pcmSamples buf))
  ErrorSpec.tests
  CodecSpec.tests
  CodecContextSpec.tests
  FrameSpec.tests
  PacketSpec.tests
  putStrLn "All tests passed!"
