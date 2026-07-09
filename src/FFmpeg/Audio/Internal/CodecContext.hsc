module FFmpeg.Audio.Internal.CodecContext
    ( CodecContextHandle
    , allocateCodecContext
    , configureCodecContext
    , openCodecContext
    , freeCodecContext
    , withCodecContext
    , getCodecContextPtr
    ) where

#include <libavcodec/avcodec.h>
#include <libavutil/channel_layout.h>
#include <libavutil/samplefmt.h>

import Control.Exception (bracket, throwIO)
import Foreign.C.Types (CInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr
    (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, pokeByteOff)

import FFmpeg.Audio.Internal.Codec (CodecHandle, getCodecPtr)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Types

data CodecContextHandle = CodecContextHandle CodecHandle (ForeignPtr AVCodecContext)

sampleRateOffset :: Int
sampleRateOffset = #{offset AVCodecContext, sample_rate}

sampleFmtOffset :: Int
sampleFmtOffset = #{offset AVCodecContext, sample_fmt}

bitRateOffset :: Int
bitRateOffset = #{offset AVCodecContext, bit_rate}

strictStdComplianceOffset :: Int
strictStdComplianceOffset = #{offset AVCodecContext, strict_std_compliance}

chLayoutOffset :: Int
chLayoutOffset = #{offset AVCodecContext, ch_layout}

setField :: Storable a => Ptr AVCodecContext -> Int -> a -> IO ()
setField ptr offset val = pokeByteOff (castPtr ptr) offset val

allocateCodecContext :: CodecHandle -> IO CodecContextHandle
allocateCodecContext codec = do
    ptr <- c_avcodec_alloc_context3 (getCodecPtr codec)
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "Failed to allocate AVCodecContext"
        else do
            let finalizer = alloca $ \pPtr -> do
                    pokeByteOff (castPtr pPtr) (0 :: Int) ptr
                    c_avcodec_free_context pPtr
            fptr <- newForeignPtr ptr finalizer
            return $ CodecContextHandle codec fptr

configureCodecContext :: CodecContextHandle -> Int -> Int -> IO ()
configureCodecContext h rate chans =
    withForeignPtr fptr $ \ptr -> do
        setField ptr sampleRateOffset (fromIntegral rate :: CInt)
        setField ptr sampleFmtOffset (fromIntegral avSampleFormatS16P :: CInt)
        setField ptr bitRateOffset (320000 :: Int)
        setField ptr strictStdComplianceOffset (-2 :: CInt)
        let chLayoutPtr = castPtr (ptr `plusPtr` chLayoutOffset)
        av_channel_layout_default chLayoutPtr (fromIntegral chans)
  where
    CodecContextHandle _ fptr = h

openCodecContext :: CodecContextHandle -> IO ()
openCodecContext h =
    withForeignPtr fptr $ \ptr -> do
        ret <- c_avcodec_open2 ptr (getCodecPtr codec) nullPtr
        _ <- errorNonNegative "avcodec_open2" (fromIntegral ret)
        return ()
  where
    CodecContextHandle codec fptr = h

freeCodecContext :: CodecContextHandle -> IO ()
freeCodecContext (CodecContextHandle _ fptr) = finalizeForeignPtr fptr

withCodecContext :: CodecHandle -> Int -> Int -> (CodecContextHandle -> IO a) -> IO a
withCodecContext codec rate chans action =
    bracket
        (allocateCodecContext codec)
        freeCodecContext
        (\h -> configureCodecContext h rate chans >> openCodecContext h >> action h)

getCodecContextPtr :: CodecContextHandle -> Ptr AVCodecContext
getCodecContextPtr (CodecContextHandle _ fptr) =
    unsafeForeignPtrToPtr fptr