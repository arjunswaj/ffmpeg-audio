module MetadataSpec (tests) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.List (isInfixOf)
import Data.Vector.Storable qualified as VS
import Data.Word (Word16, Word32)
import FFmpeg.Audio
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

tests :: IO ()
tests = do
    testEmbedMp3Id3v2EmptyNoOp
    testEncodeAudioWithMetadataMp3ByteLevel
    testEmbedMp3Id3v2ReplacesExisting
    putStrLn "All Metadata / ID3v2 SYLT tests passed!"

sineStereo :: Int -> Int -> AudioBuffer
sineStereo sr frames =
    let vec = VS.generate (frames * 2) $ \idx ->
            let f = idx `div` 2
                c = idx `mod` 2
                freq = if c == 0 then 440.0 else 550.0 :: Double
                t = fromIntegral f / fromIntegral sr :: Double
            in realToFrac (0.25 * sin (2.0 * pi * freq * t))
    in AudioBuffer sr LayoutStereo (SampleDataFloat vec)

readBeWord32 :: BS.ByteString -> Int -> Word32
readBeWord32 bs off =
    (fromIntegral (BS.index bs off) `shiftL` 24)
    .|. (fromIntegral (BS.index bs (off + 1)) `shiftL` 16)
    .|. (fromIntegral (BS.index bs (off + 2)) `shiftL` 8)
    .|. fromIntegral (BS.index bs (off + 3))

decodeUtf16LeWithBom :: BS.ByteString -> Either String String
decodeUtf16LeWithBom bs
    | BS.length bs < 2 = Left "UTF-16LE string shorter than BOM"
    | BS.index bs 0 /= 0xff || BS.index bs 1 /= 0xfe = Left "Missing UTF-16LE BOM (FF FE)"
    | odd (BS.length bs) = Left "Odd byte length in UTF-16LE string"
    | otherwise = decodeUnits (BS.drop 2 bs)
  where
    readU16 b off =
        (fromIntegral (BS.index b off) :: Word16)
        .|. (fromIntegral (BS.index b (off + 1)) `shiftL` 8)
    decodeUnits b
        | BS.null b = Right []
        | otherwise =
            let u1 = readU16 b 0
                rest = BS.drop 2 b
            in if u1 >= 0xd800 && u1 <= 0xdbff
                then if BS.length rest < 2
                    then Left "Truncated UTF-16 surrogate pair"
                    else
                        let u2 = readU16 rest 0
                            cp = 0x10000 + ((fromIntegral (u1 - 0xd800) `shiftL` 10) .|. fromIntegral (u2 - 0xdc00))
                        in fmap (chr cp :) (decodeUnits (BS.drop 2 rest))
                else fmap (chr (fromIntegral u1) :) (decodeUnits rest)

splitUtf16Null :: BS.ByteString -> Either String (BS.ByteString, BS.ByteString)
splitUtf16Null bs = go 0
  where
    n = BS.length bs
    go off
        | off + 1 >= n = Left "Unterminated UTF-16 string"
        | BS.index bs off == 0 && BS.index bs (off + 1) == 0 =
            Right (BS.take off bs, BS.drop (off + 2) bs)
        | otherwise = go (off + 2)

parseId3v23Frames :: BS.ByteString -> Either String [(String, BS.ByteString)]
parseId3v23Frames bs
    | BS.length bs < 10 = Left "MP3 shorter than ID3v2 header"
    | BS.take 3 bs /= BS.pack [0x49, 0x44, 0x33] = Left "Missing ID3 magic"
    | BS.index bs 3 /= 0x03 || BS.index bs 4 /= 0x00 = Left "Expected ID3v2.3.0"
    | BS.index bs 5 /= 0x00 = Left "Expected zero ID3v2 header flags"
    | otherwise =
        let b6 = BS.index bs 6
            b7 = BS.index bs 7
            b8 = BS.index bs 8
            b9 = BS.index bs 9
            tagSize =
                (fromIntegral (b6 .&. 0x7f) `shiftL` 21)
                .|. (fromIntegral (b7 .&. 0x7f) `shiftL` 14)
                .|. (fromIntegral (b8 .&. 0x7f) `shiftL` 7)
                .|. fromIntegral (b9 .&. 0x7f)
            body = BS.take tagSize (BS.drop 10 bs)
        in parseFrames body
  where
    parseFrames remBs
        | BS.null remBs = Right []
        | BS.length remBs < 10 = Left "Truncated ID3v2.3 frame header"
        | otherwise =
            let fid = map (chr . fromIntegral) (BS.unpack (BS.take 4 remBs))
                sz = fromIntegral (readBeWord32 remBs 4) :: Int
                flags = BS.take 2 (BS.drop 8 remBs)
                afterHdr = BS.drop 10 remBs
            in if flags /= BS.pack [0x00, 0x00]
                then Left ("Non-zero flags on frame " ++ fid)
                else if BS.length afterHdr < sz
                    then Left ("Truncated body on frame " ++ fid)
                    else fmap ((fid, BS.take sz afterHdr) :) (parseFrames (BS.drop sz afterHdr))

parseTextFrame :: BS.ByteString -> Either String String
parseTextFrame body
    | BS.null body = Left "Empty text frame"
    | BS.index body 0 /= 0x01 = Left "Expected UTF-16 BOM encoding (0x01)"
    | otherwise = decodeUtf16LeWithBom (BS.drop 1 body)

parseUsltFrame :: BS.ByteString -> Either String (String, String, String)
parseUsltFrame body
    | BS.length body < 6 = Left "USLT frame too short"
    | BS.index body 0 /= 0x01 = Left "USLT expected encoding 0x01"
    | otherwise = do
        let lang = map (chr . fromIntegral) (BS.unpack (BS.take 3 (BS.drop 1 body)))
        (descBytes, lyricsBytes) <- splitUtf16Null (BS.drop 4 body)
        desc <- decodeUtf16LeWithBom descBytes
        lyrics <- decodeUtf16LeWithBom lyricsBytes
        Right (lang, desc, lyrics)

parseSyltFrame :: BS.ByteString -> Either String (String, String, [TimedLyric])
parseSyltFrame body
    | BS.length body < 8 = Left "SYLT frame too short"
    | BS.index body 0 /= 0x01 = Left "SYLT expected encoding 0x01"
    | BS.index body 4 /= 0x02 = Left "SYLT expected millisecond timestamp format 0x02"
    | BS.index body 5 /= 0x01 = Left "SYLT expected lyrics content type 0x01"
    | otherwise = do
        let lang = map (chr . fromIntegral) (BS.unpack (BS.take 3 (BS.drop 1 body)))
        (descBytes, entriesBytes) <- splitUtf16Null (BS.drop 6 body)
        desc <- decodeUtf16LeWithBom descBytes
        entries <- parseEntries entriesBytes
        Right (lang, desc, entries)
  where
    parseEntries b
        | BS.null b = Right []
        | otherwise = do
            (txtBytes, afterTxt) <- splitUtf16Null b
            when (BS.length afterTxt < 4) $ Left "SYLT missing 4-byte timestamp"
            txt <- decodeUtf16LeWithBom txtBytes
            let ts = fromIntegral (readBeWord32 afterTxt 0) :: Int
            rest <- parseEntries (BS.drop 4 afterTxt)
            Right (TimedLyric ts txt : rest)

testEmbedMp3Id3v2EmptyNoOp :: IO ()
testEmbedMp3Id3v2EmptyNoOp = do
    let raw = BS.pack [0xff, 0xfb, 0x90, 0x00, 0x01, 0x02]
    when (embedMp3Id3v2 emptyAudioMetadata raw /= raw) $
        fail "embedMp3Id3v2 emptyAudioMetadata modified input bytes"
    putStrLn "  embedMp3Id3v2 emptyAudioMetadata no-op verified"

testEncodeAudioWithMetadataMp3ByteLevel :: IO ()
testEncodeAudioWithMetadataMp3ByteLevel = do
    let lyrics =
            [ TimedLyric 0 "First line"
            , TimedLyric 1250 "Second line \8212 \2342\2370\2360\2352\2368 \2346\2306\2325\2381\2340\2367"
            , TimedLyric 3500 "Final line \127925"
            ]
        unsynced = "First line\nSecond line \8212 \2342\2370\2360\2352\2368 \2346\2306\2325\2381\2340\2367\nFinal line \127925"
        meta = AudioMetadata
            { metaTitle = Just "Raag Yaman \8212 \2352\2366\2327 \2351\2350\2344"
            , metaArtist = Just "Antigravity Ensemble"
            , metaAlbum = Just "Spatial Sessions"
            , metaSyncedLyrics = lyrics
            , metaUnsyncedLyrics = Just unsynced
            }
        buf = sineStereo 48000 48000
    bs <- encodeAudioWithMetadataBS defaultMp3Config meta buf
    frames <- case parseId3v23Frames bs of
        Left e -> fail ("ID3v2.3 parse failed: " ++ e)
        Right fs -> return fs
    let expectFrame :: (Eq a, Show a) => String -> (BS.ByteString -> Either String a) -> a -> IO ()
        expectFrame fid parser expected =
            case lookup fid frames of
                Nothing -> fail ("Missing ID3v2.3 frame: " ++ fid)
                Just body -> case parser body of
                    Left e -> fail ("Failed parsing frame " ++ fid ++ ": " ++ e)
                    Right actual ->
                        when (actual /= expected) $
                            fail ("Frame " ++ fid ++ " mismatch: expected " ++ show expected ++ ", got " ++ show actual)
    expectFrame "TIT2" parseTextFrame "Raag Yaman \8212 \2352\2366\2327 \2351\2350\2344"
    expectFrame "TPE1" parseTextFrame "Antigravity Ensemble"
    expectFrame "TALB" parseTextFrame "Spatial Sessions"
    expectFrame "USLT" parseUsltFrame ("eng", "", unsynced)
    expectFrame "SYLT" parseSyltFrame ("eng", "", lyrics)
    putStrLn "  Byte-level ID3v2.3 TIT2/TPE1/TALB/USLT/SYLT verification passed"
    (fp, h) <- openTempFile "/tmp" "ffmpeg-meta-test.mp3"
    hClose h
    finally
        (do
            encodeAudioWithMetadata fp defaultMp3Config meta buf
            (ecP, outP, errP) <- readProcessWithExitCode "ffprobe"
                [ "-v", "warning"
                , "-show_entries", "format_tags"
                , "-of", "default=noprint_wrappers=1"
                , fp
                ] ""
            case ecP of
                ExitFailure c -> fail ("ffprobe MP3 metadata failed (" ++ show c ++ "): " ++ errP)
                ExitSuccess -> do
                    unless (null errP) $
                        fail ("ffprobe reported warnings on MP3 ID3v2.3:\n" ++ errP)
                    unless ("TAG:title=Raag Yaman" `isInfixOf` outP) $
                        fail ("ffprobe missing title tag:\n" ++ outP)
                    unless ("TAG:artist=Antigravity Ensemble" `isInfixOf` outP) $
                        fail ("ffprobe missing artist tag:\n" ++ outP)
                    unless ("TAG:album=Spatial Sessions" `isInfixOf` outP) $
                        fail ("ffprobe missing album tag:\n" ++ outP)
                    unless ("TAG:lyrics-eng=First line" `isInfixOf` outP) $
                        fail ("ffprobe missing lyrics-eng tag:\n" ++ outP)
            (ecD, _, errD) <- readProcessWithExitCode "ffmpeg"
                [ "-v", "error"
                , "-i", fp
                , "-f", "null"
                , "-"
                ] ""
            case ecD of
                ExitFailure c -> fail ("ffmpeg decode of tagged MP3 failed (" ++ show c ++ "): " ++ errD)
                ExitSuccess ->
                    unless (null errD) $
                        fail ("ffmpeg decode of tagged MP3 emitted stderr:\n" ++ errD)
            putStrLn "  ffprobe + ffmpeg zero-warning verification passed for tagged MP3")
        (removeFile fp)

testEmbedMp3Id3v2ReplacesExisting :: IO ()
testEmbedMp3Id3v2ReplacesExisting = do
    let buf = sineStereo 48000 24000
        meta1 = emptyAudioMetadata
            { metaTitle = Just "Old Title"
            , metaSyncedLyrics = [TimedLyric 100 "Old"]
            }
        meta2 = emptyAudioMetadata
            { metaTitle = Just "New Title"
            , metaSyncedLyrics = [TimedLyric 500 "New"]
            }
    bs1 <- encodeAudioWithMetadataBS defaultMp3Config meta1 buf
    let bs2 = embedMp3Id3v2 meta2 bs1
    frames <- case parseId3v23Frames bs2 of
        Left e -> fail ("Replaced ID3v2.3 parse failed: " ++ e)
        Right fs -> return fs
    case lookup "TIT2" frames >>= either (const Nothing) Just . parseTextFrame of
        Just "New Title" -> return ()
        other -> fail ("Expected replaced TIT2 = New Title, got " ++ show other)
    case lookup "SYLT" frames >>= either (const Nothing) Just . parseSyltFrame of
        Just ("eng", "", [TimedLyric 500 "New"]) -> return ()
        other -> fail ("Expected replaced SYLT, got " ++ show other)
    let id3HeaderSize =
            let b6 = BS.index bs2 6
                b7 = BS.index bs2 7
                b8 = BS.index bs2 8
                b9 = BS.index bs2 9
            in 10
                + (fromIntegral (b6 .&. 0x7f) `shiftL` 21)
                + (fromIntegral (b7 .&. 0x7f) `shiftL` 14)
                + (fromIntegral (b8 .&. 0x7f) `shiftL` 7)
                + fromIntegral (b9 .&. 0x7f)
        afterTag = BS.drop id3HeaderSize bs2
    when (BS.take 3 afterTag == BS.pack [0x49, 0x44, 0x33]) $
        fail "embedMp3Id3v2 stacked duplicate ID3v2 headers instead of replacing"
    putStrLn "  embedMp3Id3v2 replacement (no duplicate ID3 headers) verified"
