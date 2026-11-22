module FFmpeg.Audio.Metadata
    ( TimedLyric(..)
    , AudioMetadata(..)
    , emptyAudioMetadata
    , embedMp3Id3v2
    , encodeUtf8
    , resolvedPlainLyrics
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Char (ord)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Word (Word16, Word32)

data TimedLyric = TimedLyric
    { tlTimestampMs :: !Int
    , tlText        :: !String
    } deriving (Show, Eq)

data AudioMetadata = AudioMetadata
    { metaTitle          :: !(Maybe String)
    , metaArtist         :: !(Maybe String)
    , metaAlbum          :: !(Maybe String)
    , metaSyncedLyrics   :: ![TimedLyric]
    , metaUnsyncedLyrics :: !(Maybe String)
    } deriving (Show, Eq)

emptyAudioMetadata :: AudioMetadata
emptyAudioMetadata = AudioMetadata
    { metaTitle = Nothing
    , metaArtist = Nothing
    , metaAlbum = Nothing
    , metaSyncedLyrics = []
    , metaUnsyncedLyrics = Nothing
    }

resolvedPlainLyrics :: AudioMetadata -> Maybe String
resolvedPlainLyrics meta = case metaUnsyncedLyrics meta of
    Just txt | not (null txt) -> Just txt
    _ -> case filter (not . null . tlText) (metaSyncedLyrics meta) of
        [] -> Nothing
        ls -> Just (intercalate "\n" (map tlText ls))

toStrictBytes :: BSB.Builder -> BS.ByteString
toStrictBytes = BSL.toStrict . BSB.toLazyByteString

encodeUtf8 :: String -> BS.ByteString
encodeUtf8 = toStrictBytes . BSB.stringUtf8

utf16LeWithBomBuilder :: String -> BSB.Builder
utf16LeWithBomBuilder str =
    BSB.word8 0xff <> BSB.word8 0xfe <> foldMap charToUtf16Le str
  where
    charToUtf16Le :: Char -> BSB.Builder
    charToUtf16Le ch =
        let !cp = ord ch
        in if cp <= 0xffff
            then BSB.word16LE (fromIntegral cp :: Word16)
            else
                let !u = cp - 0x10000
                    !hi = fromIntegral (0xd800 + (u `shiftR` 10)) :: Word16
                    !lo = fromIntegral (0xdc00 + (u .&. 0x3ff)) :: Word16
                in BSB.word16LE hi <> BSB.word16LE lo

synchsafe32Builder :: Int -> BSB.Builder
synchsafe32Builder n =
    let !v = max 0 n
    in BSB.word8 (fromIntegral ((v `shiftR` 21) .&. 0x7f))
        <> BSB.word8 (fromIntegral ((v `shiftR` 14) .&. 0x7f))
        <> BSB.word8 (fromIntegral ((v `shiftR` 7) .&. 0x7f))
        <> BSB.word8 (fromIntegral (v .&. 0x7f))

buildId3Frame :: String -> BS.ByteString -> BSB.Builder
buildId3Frame fid body =
    BSB.string7 (take 4 fid)
        <> BSB.word32BE (fromIntegral (BS.length body) :: Word32)
        <> BSB.word16BE 0
        <> BSB.byteString body

buildTextFrame :: String -> String -> BSB.Builder
buildTextFrame fid txt =
    buildId3Frame fid (toStrictBytes (BSB.word8 0x01 <> utf16LeWithBomBuilder txt))

buildUsltFrame :: String -> BSB.Builder
buildUsltFrame txt =
    buildId3Frame "USLT" $
        toStrictBytes $
            BSB.word8 0x01
                <> BSB.string7 "eng"
                <> BSB.word8 0xff
                <> BSB.word8 0xfe
                <> BSB.word16LE 0
                <> utf16LeWithBomBuilder txt

buildSyltFrame :: [TimedLyric] -> BSB.Builder
buildSyltFrame lyrics =
    buildId3Frame "SYLT" $
        toStrictBytes $
            BSB.word8 0x01
                <> BSB.string7 "eng"
                <> BSB.word8 0x02
                <> BSB.word8 0x01
                <> BSB.word8 0xff
                <> BSB.word8 0xfe
                <> BSB.word16LE 0
                <> foldMap encodeEntry lyrics
  where
    encodeEntry (TimedLyric ts txt) =
        utf16LeWithBomBuilder txt
            <> BSB.word16LE 0
            <> BSB.word32BE (fromIntegral (max 0 ts) :: Word32)

stripExistingId3v2 :: BS.ByteString -> BS.ByteString
stripExistingId3v2 bs
    | BS.length bs < 10 = bs
    | BS.index bs 0 /= 0x49 || BS.index bs 1 /= 0x44 || BS.index bs 2 /= 0x33 = bs
    | otherwise =
        let !flags = BS.index bs 5
            !b6 = BS.index bs 6
            !b7 = BS.index bs 7
            !b8 = BS.index bs 8
            !b9 = BS.index bs 9
        in if (b6 .|. b7 .|. b8 .|. b9) .&. 0x80 /= 0
            then bs
            else
                let !tagSize =
                        (fromIntegral b6 `shiftL` 21)
                        .|. (fromIntegral b7 `shiftL` 14)
                        .|. (fromIntegral b8 `shiftL` 7)
                        .|. fromIntegral b9
                    !footerSize = if (flags .&. 0x10) /= 0 then 10 else 0
                    !totalSkip = 10 + tagSize + footerSize
                in if totalSkip <= BS.length bs
                    then BS.drop totalSkip bs
                    else bs

embedMp3Id3v2 :: AudioMetadata -> BS.ByteString -> BS.ByteString
embedMp3Id3v2 meta mp3Bytes =
    let mFrame _ Nothing = Nothing
        mFrame f (Just s)
            | null s = Nothing
            | otherwise = Just (f s)
        syltFrame =
            if null (metaSyncedLyrics meta)
                then Nothing
                else Just (buildSyltFrame (metaSyncedLyrics meta))
        frames = catMaybes
            [ mFrame (buildTextFrame "TIT2") (metaTitle meta)
            , mFrame (buildTextFrame "TPE1") (metaArtist meta)
            , mFrame (buildTextFrame "TALB") (metaAlbum meta)
            , mFrame buildUsltFrame (metaUnsyncedLyrics meta)
            , syltFrame
            ]
    in if null frames
        then mp3Bytes
        else
            let payload = toStrictBytes (mconcat frames)
                stripped = stripExistingId3v2 mp3Bytes
            in toStrictBytes $
                BSB.string7 "ID3"
                    <> BSB.word8 0x03
                    <> BSB.word8 0x00
                    <> BSB.word8 0x00
                    <> synchsafe32Builder (BS.length payload)
                    <> BSB.byteString payload
                    <> BSB.byteString stripped
