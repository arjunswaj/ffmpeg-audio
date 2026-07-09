module FFmpeg.Audio.Internal.Format
    ( FormatContextHandle
    , StreamHandle
    , allocateOutputContext
    , openOutput
    , createStream
    , setCodecParameters
    , writeHeader
    , writeFrame
    , writeTrailer
    , closeOutput
    , freeFormatContext
    , withOutputFile
    , withFormatContext
    ) where

#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>

import Control.Exception (bracket, catch, throwIO, SomeException)
import Foreign.C.String (withCString)

import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr
    (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import FFmpeg.Audio.Internal.Codec (CodecHandle, getCodecPtr)
import FFmpeg.Audio.Internal.CodecContext
    (CodecContextHandle, getCodecContextPtr)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Packet (PacketHandle, getPacketPtr)
import FFmpeg.Audio.Internal.Types

newtype FormatContextHandle = FormatContextHandle (ForeignPtr AVFormatContext)
newtype StreamHandle = StreamHandle (Ptr AVStream)

allocateOutputContext :: FilePath -> IO FormatContextHandle
allocateOutputContext outputPath =
    alloca $ \pFmtCtx -> do
        withCString outputPath $ \cPath -> do
            withCString "mp3" $ \fmtName -> do
                err <- c_avformat_alloc_output_context2 pFmtCtx nullPtr fmtName cPath
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
    withForeignPtr fptr $ \ctxPtr -> do
        alloca $ \pPb -> do
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
            then do
                alloca $ \pPb -> do
                    pokeByteOff (castPtr pPb) (0 :: Int) pb
                    _ <- c_avio_closep pPb
                    pokeByteOff (castPtr ctxPtr) formatPbOffset (nullPtr :: Ptr AVIOContext)
                    return ()
            else return ()

freeFormatContext :: FormatContextHandle -> IO ()
freeFormatContext (FormatContextHandle fptr) = finalizeForeignPtr fptr

withFormatContext :: FilePath -> (FormatContextHandle -> IO a) -> IO a
withFormatContext outputPath action =
    bracket
        (allocateOutputContext outputPath)
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