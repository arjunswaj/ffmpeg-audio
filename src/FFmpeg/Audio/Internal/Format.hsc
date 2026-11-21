module FFmpeg.Audio.Internal.Format
    ( FormatContextHandle
    , StreamHandle
    , allocateOutputContext
    , allocateOutputContextWith
    , openOutput
    , createStream
    , setCodecParameters
    , writeHeader
    , writeFrame
    , rescaleAndWriteFrame
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
    , formatNeedsGlobalHeader
    , setFormatStrictStdCompliance
    ) where

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>

import Control.Exception (bracket, bracket_, catch, throwIO, SomeException)
import Data.Bits ((.&.))
import Data.Int (Int64)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr
    (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import FFmpeg.Audio.Internal.Codec (CodecHandle, getCodecPtr)
import FFmpeg.Audio.Internal.CodecContext
    (CodecContextHandle, getCodecContextPtr, getCodecTimeBase)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Packet (PacketHandle, getPacketPtr)
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

writeHeader :: FormatContextHandle -> IO ()
writeHeader (FormatContextHandle fptr) =
    withForeignPtr fptr $ \ctxPtr -> do
        err <- c_avformat_write_header ctxPtr nullPtr
        _ <- errorNonNegative "avformat_write_header" (fromIntegral err)
        return ()

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
