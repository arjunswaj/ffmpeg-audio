module FFmpeg.Audio.Internal.Format
    ( FormatContextHandle
    , StreamHandle
    , allocateOutputContext
    , allocateOutputContextWith
    , openOutput
    , createStream
    , createRawStream
    , setCodecParameters
    , configureMovTextStream
    , writeHeader
    , writeHeaderWithOptions
    , writeFrame
    , rescaleAndWriteFrame
    , writeMovTextPacket
    , writeTrailer
    , closeOutput
    , freeFormatContext
    , withOutputFile
    , withOutputFileWith
    , withFormatContext
    , withFormatContextWith
    , getFormatContextPtr
    , getStreamPtr
    , getStreamIndex
    , setStreamId
    , setStreamTimeBase
    , getStreamTimeBase
    , setStreamDisposition
    , setStreamAvgFrameRate
    , setStreamMetadata
    , setFormatMetadata
    , formatNeedsGlobalHeader
    , setFormatStrictStdCompliance
    ) where

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>

import Control.Exception (bracket, bracket_, catch, finally, mask_, throwIO, SomeException)
import Control.Monad (forM_)
import Data.Bits ((.&.), shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64)
import Data.Word (Word32, Word8)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr
    (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff, pokeElemOff)

import FFmpeg.Audio.Internal.Codec (CodecHandle, getCodecPtr)
import FFmpeg.Audio.Internal.CodecContext
    (CodecContextHandle, getCodecContextPtr, getCodecTimeBase)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Packet (PacketHandle, getPacketPtr, unrefPacket, withPacket)
import FFmpeg.Audio.Internal.Types

newtype FormatContextHandle = FormatContextHandle (ForeignPtr AVFormatContext)
newtype StreamHandle = StreamHandle (Ptr AVStream)

allocateOutputContext :: FilePath -> IO FormatContextHandle
allocateOutputContext = allocateOutputContextWith (Just "mp3")

allocateOutputContextWith :: Maybe String -> FilePath -> IO FormatContextHandle
allocateOutputContextWith mFmtName outputPath =
    alloca $ \pFmtCtx ->
        withCString outputPath $ \cPath -> do
            err <- case mFmtName of
                Just fmtName ->
                    withCString fmtName $ \cFmt ->
                        c_avformat_alloc_output_context2 pFmtCtx nullPtr cFmt cPath
                Nothing ->
                    c_avformat_alloc_output_context2 pFmtCtx nullPtr nullPtr cPath
            _ <- errorNonNegative "avformat_alloc_output_context2" (fromIntegral err)
            ptr <- peekByteOff (castPtr pFmtCtx) (0 :: Int) :: IO (Ptr AVFormatContext)
            if ptr == nullPtr
                then throwIO $ FFmpegError (-1) "allocateOutputContext: null format context"
                else do
                    let finalizer = c_avformat_free_context ptr
                    fptr <- newForeignPtr ptr finalizer
                    return $ FormatContextHandle fptr

openOutput :: FormatContextHandle -> FilePath -> IO ()
openOutput (FormatContextHandle fptr) outputPath =
    withForeignPtr fptr $ \ctxPtr ->
        alloca $ \pPb ->
            withCString outputPath $ \cPath -> do
                err <- c_avio_open pPb cPath (fromIntegral avioFlagWrite)
                _ <- errorNonNegative "avio_open" (fromIntegral err)
                pbPtr <- peekByteOff (castPtr pPb) (0 :: Int) :: IO (Ptr AVIOContext)
                pokeByteOff (castPtr ctxPtr) formatPbOffset pbPtr

createStream :: FormatContextHandle -> CodecHandle -> IO StreamHandle
createStream (FormatContextHandle fptr) codec =
    withForeignPtr fptr $ \ctxPtr -> do
        ptr <- c_avformat_new_stream ctxPtr (getCodecPtr codec)
        if ptr == nullPtr
            then throwIO $ FFmpegError (-1) "createStream: avformat_new_stream returned null"
            else return $ StreamHandle ptr

createRawStream :: FormatContextHandle -> IO StreamHandle
createRawStream (FormatContextHandle fptr) =
    withForeignPtr fptr $ \ctxPtr -> do
        ptr <- c_avformat_new_stream ctxPtr nullPtr
        if ptr == nullPtr
            then throwIO $ FFmpegError (-1) "createRawStream: avformat_new_stream returned null"
            else return $ StreamHandle ptr

setCodecParameters :: StreamHandle -> CodecContextHandle -> IO ()
setCodecParameters (StreamHandle sPtr) cch = do
    let ccPtr = getCodecContextPtr cch
    codecpar <- peekByteOff (castPtr sPtr) streamCodecparOffset :: IO (Ptr AVCodecParameters)
    if codecpar == nullPtr
        then throwIO $ FFmpegError (-1) "setCodecParameters: stream has no codecpar"
        else do
            err <- c_avcodec_parameters_from_context codecpar ccPtr
            _ <- errorNonNegative "avcodec_parameters_from_context" (fromIntegral err)
            return ()

movTextExtradata :: [Word8]
movTextExtradata =
    [ 0x00, 0x00, 0x00, 0x00, 0x01, 0xff, 0x00, 0x00
    , 0x00, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01
    , 0x00, 0x10, 0xff, 0xff, 0xff, 0xff, 0x00, 0x00
    , 0x00, 0x12, 0x66, 0x74, 0x61, 0x62, 0x00, 0x01
    , 0x00, 0x01, 0x05, 0x41, 0x72, 0x69, 0x61, 0x6c
    ]

configureMovTextStream :: StreamHandle -> IO ()
configureMovTextStream (StreamHandle sPtr) = do
    codecpar <- peekByteOff (castPtr sPtr) streamCodecparOffset :: IO (Ptr AVCodecParameters)
    if codecpar == nullPtr
        then throwIO $ FFmpegError (-1) "configureMovTextStream: stream has no codecpar"
        else mask_ $ do
            let extraLen = length movTextExtradata
            rawBuf <- c_av_mallocz (fromIntegral (extraLen + avInputBufferPaddingSize))
            if rawBuf == nullPtr
                then throwIO $ FFmpegError (-1) "configureMovTextStream: av_mallocz failed"
                else do
                    let byteBuf = castPtr rawBuf :: Ptr Word8
                    forM_ (zip [0 ..] movTextExtradata) $ \(i, b) ->
                        pokeElemOff byteBuf i b
                    pokeByteOff (castPtr codecpar) codecparCodecTypeOffset avMediaTypeSubtitle
                    pokeByteOff (castPtr codecpar) codecparCodecIdOffset avCodecIdMovText
                    pokeByteOff (castPtr codecpar) codecparCodecTagOffset (0 :: Word32)
                    pokeByteOff (castPtr codecpar) codecparExtradataOffset byteBuf
                    pokeByteOff (castPtr codecpar) codecparExtradataSizeOffset (fromIntegral extraLen :: CInt)
                    setStreamTimeBase (StreamHandle sPtr) (AVRational 1 1000)

writeHeader :: FormatContextHandle -> IO ()
writeHeader fmt = writeHeaderWithOptions fmt []

writeHeaderWithOptions :: FormatContextHandle -> [(String, String)] -> IO ()
writeHeaderWithOptions (FormatContextHandle fptr) opts =
    withForeignPtr fptr $ \ctxPtr ->
        if null opts
            then do
                err <- c_avformat_write_header ctxPtr nullPtr
                _ <- errorNonNegative "avformat_write_header" (fromIntegral err)
                return ()
            else
                alloca $ \pDict -> do
                    pokeByteOff (castPtr pDict) (0 :: Int) (nullPtr :: Ptr AVDictionary)
                    finally
                        (do
                            forM_ opts $ \(k, v) ->
                                withCString k $ \cKey ->
                                    withCString v $ \cVal -> do
                                        r <- c_av_dict_set pDict cKey cVal 0
                                        _ <- errorNonNegative ("av_dict_set(" ++ k ++ ")") (fromIntegral r)
                                        return ()
                            err <- c_avformat_write_header ctxPtr (castPtr pDict)
                            _ <- errorNonNegative "avformat_write_header" (fromIntegral err)
                            return ())
                        (c_av_dict_free pDict)

writeFrame :: FormatContextHandle -> PacketHandle -> IO ()
writeFrame (FormatContextHandle fptr) ph =
    withForeignPtr fptr $ \ctxPtr -> do
        err <- c_av_interleaved_write_frame ctxPtr (getPacketPtr ph)
        _ <- errorNonNegative "av_interleaved_write_frame" (fromIntegral err)
        return ()

rescaleTimestamp :: Int64 -> AVRational -> AVRational -> Int64
rescaleTimestamp val (AVRational sn sd) (AVRational dn dd)
    | val == avNoptsValue = avNoptsValue
    | sd == 0 || dn == 0 = val
    | otherwise =
        let num = toInteger val * toInteger sn * toInteger dd
            den = toInteger sd * toInteger dn
        in fromIntegral (num `quot` den)

rescaleAndWriteFrame :: FormatContextHandle -> StreamHandle -> AVRational -> PacketHandle -> IO ()
rescaleAndWriteFrame fmt sh srcTb ph = do
    let pktPtr = getPacketPtr ph
    dstTb <- getStreamTimeBase sh
    sIdx <- getStreamIndex sh
    pts <- peekByteOff (castPtr pktPtr) packetPtsOffset :: IO Int64
    dts <- peekByteOff (castPtr pktPtr) packetDtsOffset :: IO Int64
    dur <- peekByteOff (castPtr pktPtr) packetDurationOffset :: IO Int64
    pokeByteOff (castPtr pktPtr) packetPtsOffset (rescaleTimestamp pts srcTb dstTb)
    pokeByteOff (castPtr pktPtr) packetDtsOffset (rescaleTimestamp dts srcTb dstTb)
    pokeByteOff (castPtr pktPtr) packetDurationOffset (rescaleTimestamp dur srcTb dstTb)
    pokeByteOff (castPtr pktPtr) packetStreamIndexOffset (fromIntegral sIdx :: CInt)
    writeFrame fmt ph

writeMovTextPacket :: FormatContextHandle -> StreamHandle -> Int64 -> Int64 -> BS.ByteString -> IO ()
writeMovTextPacket fmt sh ptsMs durMs utf8Text =
    withPacket $ \ph -> do
        let pktPtr = getPacketPtr ph
            textLen = min 65535 (BS.length utf8Text)
            pktSize = 2 + textLen
        ret <- c_av_new_packet pktPtr (fromIntegral pktSize)
        _ <- errorNonNegative "av_new_packet(mov_text)" (fromIntegral ret)
        finally
            (do
                dataPtr <- peekByteOff (castPtr pktPtr) packetDataOffset :: IO (Ptr Word8)
                pokeElemOff dataPtr 0 (fromIntegral ((textLen `shiftR` 8) .&. 0xff))
                pokeElemOff dataPtr 1 (fromIntegral (textLen .&. 0xff))
                BSU.unsafeUseAsCString utf8Text $ \srcPtr ->
                    copyBytes (dataPtr `plusPtr` 2) (castPtr srcPtr) textLen
                pokeByteOff (castPtr pktPtr) packetPtsOffset ptsMs
                pokeByteOff (castPtr pktPtr) packetDtsOffset ptsMs
                pokeByteOff (castPtr pktPtr) packetDurationOffset durMs
                pokeByteOff (castPtr pktPtr) packetFlagsOffset avPktFlagKey
                rescaleAndWriteFrame fmt sh (AVRational 1 1000) ph)
            (unrefPacket ph)

writeTrailer :: FormatContextHandle -> IO ()
writeTrailer (FormatContextHandle fptr) =
    withForeignPtr fptr $ \ctxPtr -> do
        err <- c_av_write_trailer ctxPtr
        _ <- errorNonNegative "av_write_trailer" (fromIntegral err)
        return ()

closeOutput :: FormatContextHandle -> IO ()
closeOutput (FormatContextHandle fptr) =
    withForeignPtr fptr $ \ctxPtr -> do
        pb <- peekByteOff (castPtr ctxPtr) formatPbOffset :: IO (Ptr AVIOContext)
        if pb /= nullPtr
            then
                alloca $ \pPb -> do
                    pokeByteOff (castPtr pPb) (0 :: Int) pb
                    _ <- c_avio_closep pPb
                    pokeByteOff (castPtr ctxPtr) formatPbOffset (nullPtr :: Ptr AVIOContext)
                    return ()
            else return ()

freeFormatContext :: FormatContextHandle -> IO ()
freeFormatContext (FormatContextHandle fptr) = finalizeForeignPtr fptr

withFormatContext :: FilePath -> (FormatContextHandle -> IO a) -> IO a
withFormatContext = withFormatContextWith (Just "mp3")

withFormatContextWith :: Maybe String -> FilePath -> (FormatContextHandle -> IO a) -> IO a
withFormatContextWith mFmtName outputPath action =
    bracket
        (allocateOutputContextWith mFmtName outputPath)
        freeFormatContext
        action

withOutputFile :: FilePath -> CodecHandle -> CodecContextHandle -> (FormatContextHandle -> IO a) -> IO a
withOutputFile outputPath codeh cch action =
    bracket
        (do
            fmt <- allocateOutputContext outputPath
            openOutput fmt outputPath
            sh <- createStream fmt codeh
            setCodecParameters sh cch
            writeHeader fmt
            return fmt)
        (\fmt -> do
            writeTrailer fmt `catch` (\(_ :: SomeException) -> return ())
            closeOutput fmt `catch` (\(_ :: SomeException) -> return ())
            freeFormatContext fmt)
        action

withOutputFileWith :: Maybe String -> FilePath -> CodecHandle -> CodecContextHandle -> (FormatContextHandle -> StreamHandle -> IO a) -> IO a
withOutputFileWith mFmtName outputPath codeh cch action =
    withFormatContextWith mFmtName outputPath $ \fmt ->
        bracket_
            (openOutput fmt outputPath)
            (closeOutput fmt `catch` (\(_ :: SomeException) -> return ()))
            (do
                sh <- createStream fmt codeh
                tb <- getCodecTimeBase cch
                setStreamTimeBase sh tb
                setCodecParameters sh cch
                writeHeader fmt
                res <- action fmt sh
                writeTrailer fmt
                return res)

getFormatContextPtr :: FormatContextHandle -> Ptr AVFormatContext
getFormatContextPtr (FormatContextHandle fptr) = unsafeForeignPtrToPtr fptr

getStreamPtr :: StreamHandle -> Ptr AVStream
getStreamPtr (StreamHandle p) = p

getStreamIndex :: StreamHandle -> IO Int
getStreamIndex (StreamHandle p) = do
    idx <- peekByteOff (castPtr p) streamIndexOffset :: IO CInt
    return (fromIntegral idx)

setStreamId :: StreamHandle -> Int -> IO ()
setStreamId (StreamHandle p) sid =
    pokeByteOff (castPtr p) streamIdOffset (fromIntegral sid :: CInt)

setStreamTimeBase :: StreamHandle -> AVRational -> IO ()
setStreamTimeBase (StreamHandle p) tb =
    pokeByteOff (castPtr p) streamTimeBaseOffset tb

getStreamTimeBase :: StreamHandle -> IO AVRational
getStreamTimeBase (StreamHandle p) =
    peekByteOff (castPtr p) streamTimeBaseOffset

setStreamDisposition :: StreamHandle -> CInt -> IO ()
setStreamDisposition (StreamHandle p) disp =
    pokeByteOff (castPtr p) streamDispositionOffset disp

setStreamAvgFrameRate :: StreamHandle -> AVRational -> IO ()
setStreamAvgFrameRate (StreamHandle p) rate =
    pokeByteOff (castPtr p) streamAvgFrameRateOffset rate

setStreamMetadata :: StreamHandle -> String -> String -> IO ()
setStreamMetadata (StreamHandle p) key val = do
    let dictPtrPtr = castPtr (p `plusPtr` streamMetadataOffset) :: Ptr (Ptr AVDictionary)
    withCString key $ \cKey ->
        withCString val $ \cVal -> do
            ret <- c_av_dict_set dictPtrPtr cKey cVal 0
            _ <- errorNonNegative ("av_dict_set(stream:" ++ key ++ ")") (fromIntegral ret)
            return ()

setFormatMetadata :: FormatContextHandle -> String -> String -> IO ()
setFormatMetadata (FormatContextHandle fptr) key val =
    withForeignPtr fptr $ \ctxPtr -> do
        let dictPtrPtr = castPtr (ctxPtr `plusPtr` formatMetadataOffset) :: Ptr (Ptr AVDictionary)
        withCString key $ \cKey ->
            withCString val $ \cVal -> do
                ret <- c_av_dict_set dictPtrPtr cKey cVal 0
                _ <- errorNonNegative ("av_dict_set(format:" ++ key ++ ")") (fromIntegral ret)
                return ()

formatNeedsGlobalHeader :: FormatContextHandle -> IO Bool
formatNeedsGlobalHeader (FormatContextHandle fptr) =
    withForeignPtr fptr $ \ctxPtr -> do
        ofmt <- peekByteOff (castPtr ctxPtr) formatOformatOffset :: IO (Ptr AVOutputFormat)
        if ofmt == nullPtr
            then return False
            else do
                fl <- peekByteOff (castPtr ofmt) oformatFlagsOffset :: IO CInt
                return ((fl .&. avFmtGlobalHeader) /= 0)

setFormatStrictStdCompliance :: FormatContextHandle -> Int -> IO ()
setFormatStrictStdCompliance (FormatContextHandle fptr) val =
    withForeignPtr fptr $ \ctxPtr ->
        pokeByteOff (castPtr ctxPtr) formatStrictStdComplianceOffset (fromIntegral val :: CInt)
