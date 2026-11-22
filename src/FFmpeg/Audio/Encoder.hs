module FFmpeg.Audio.Encoder
    ( encodeMp3
    , encodeMp3LBS
    , AudioCodec(..)
    , CodecConfig(..)
    , defaultMp3Config
    , defaultFlac24Config
    , defaultOpusConfig
    , defaultOpusAmbisonicsConfig
    , defaultAacConfig
    , defaultEac3Config
    , encodeAudio
    , encodeAudioBS
    , encodeAudioWithMetadata
    , encodeAudioWithMetadataBS
    , encodeFloatMp3
    , encodeFlac24
    , encodeOpus
    , TimedLyric(..)
    , AudioMetadata(..)
    , emptyAudioMetadata
    , embedMp3Id3v2
    ) where

import Control.Exception (SomeException, catch, finally)
import Data.ByteString qualified as BS
import FFmpeg.Audio.Internal.Encoder
    ( AudioCodec(..)
    , CodecConfig(..)
    , defaultAacConfig
    , defaultEac3Config
    , defaultExtensionFor
    , defaultFlac24Config
    , defaultMp3Config
    , defaultOpusAmbisonicsConfig
    , defaultOpusConfig
    , encodeAudioToFile
    , encodeAudioToFileWithMetadata
    , encodePcmToFile
    )
import FFmpeg.Audio.Metadata
    ( AudioMetadata(..)
    , TimedLyric(..)
    , embedMp3Id3v2
    , emptyAudioMetadata
    )
import FFmpeg.Audio.PCMBuffer
    ( AudioBuffer
    , PCMBuffer
    , PCMBufferFloat
    , fromPCMBufferFloat
    )
import System.Directory (removeFile)
import System.IO (hClose, openTempFile)

encodeMp3 :: FilePath -> PCMBuffer -> IO ()
encodeMp3 = encodePcmToFile

encodeMp3LBS :: PCMBuffer -> IO BS.ByteString
encodeMp3LBS buf = do
    (path, h) <- openTempFile "/tmp" "ffmpeg-audio.mp3"
    hClose h
    finally
        (encodeMp3 path buf >> BS.readFile path)
        (removeFile path `catch` (\(_ :: SomeException) -> return ()))

encodeAudio :: FilePath -> CodecConfig -> AudioBuffer -> IO ()
encodeAudio = encodeAudioToFile

encodeAudioBS :: CodecConfig -> AudioBuffer -> IO BS.ByteString
encodeAudioBS cfg buf = do
    let ext = case codecFormatName cfg of
            Just f -> '.' : f
            Nothing -> defaultExtensionFor (codecType cfg)
    (path, h) <- openTempFile "/tmp" ("ffmpeg-audio" ++ ext)
    hClose h
    finally
        (encodeAudio path cfg buf >> BS.readFile path)
        (removeFile path `catch` (\(_ :: SomeException) -> return ()))

encodeAudioWithMetadata :: FilePath -> CodecConfig -> AudioMetadata -> AudioBuffer -> IO ()
encodeAudioWithMetadata = encodeAudioToFileWithMetadata

encodeAudioWithMetadataBS :: CodecConfig -> AudioMetadata -> AudioBuffer -> IO BS.ByteString
encodeAudioWithMetadataBS cfg meta buf = do
    let ext = case codecFormatName cfg of
            Just f -> '.' : f
            Nothing -> defaultExtensionFor (codecType cfg)
    (path, h) <- openTempFile "/tmp" ("ffmpeg-audio" ++ ext)
    hClose h
    finally
        (encodeAudioWithMetadata path cfg meta buf >> BS.readFile path)
        (removeFile path `catch` (\(_ :: SomeException) -> return ()))

encodeFloatMp3 :: FilePath -> PCMBufferFloat -> IO ()
encodeFloatMp3 path buf = encodeAudio path defaultMp3Config (fromPCMBufferFloat buf)

encodeFlac24 :: FilePath -> AudioBuffer -> IO ()
encodeFlac24 path = encodeAudio path defaultFlac24Config

encodeOpus :: FilePath -> CodecConfig -> AudioBuffer -> IO ()
encodeOpus = encodeAudio
