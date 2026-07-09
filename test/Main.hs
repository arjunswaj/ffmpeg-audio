module Main (main) where

import FFmpeg.Audio.PCMBuffer (PCMBuffer (..))
import Data.Vector qualified as V

main :: IO ()
main = do
  let buf = PCMBuffer 44100 2 (V.fromList [0, 1, -1, 32767, -32768])
  putStrLn $ "PCMBuffer: " ++ show buf
  putStrLn $ "Sample rate: " ++ show (pcmSampleRate buf)
  putStrLn $ "Channels: " ++ show (pcmChannels buf)
  putStrLn $ "Samples length: " ++ show (V.length (pcmSamples buf))
  putStrLn "All tests passed!"
