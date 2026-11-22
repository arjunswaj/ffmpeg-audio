module FFmpeg.Audio.Internal.Frame
    ( FrameHandle
    , allocateFrame
    , getFrameBuffer
    , makeFrameWritable
    , freeFrame
    , withFrame
    , withFrameWithLayout
    , withVideoFrame
    , withEmptyFrame
    , setFrameParams
    , setFrameParamsWithLayout
    , setVideoFrameParams
    , getFramePtr
    ) where

#include <libavutil/frame.h>
#include <libavutil/samplefmt.h>

import Control.Exception (bracket, throwIO)
import Foreign.C.Types (CInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr
    (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (pokeByteOff)

import FFmpeg.Audio.Internal.CodecContext (initChannelLayout)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Types
import FFmpeg.Audio.PCMBuffer (ChannelLayout)

newtype FrameHandle = FrameHandle (ForeignPtr AVFrame)

allocateFrame :: IO FrameHandle
allocateFrame = do
    ptr <- c_av_frame_alloc
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "Failed to allocate AVFrame"
        else do
            let finalizer = alloca $ \pPtr -> do
                    pokeByteOff (castPtr pPtr) (0 :: Int) ptr
                    c_av_frame_free pPtr
            fptr <- newForeignPtr ptr finalizer
            return $ FrameHandle fptr

setFrameParams :: FrameHandle -> Int -> Int -> Int -> AVSampleFormat -> IO ()
setFrameParams (FrameHandle fptr) nsamples rate chans fmt =
    withForeignPtr fptr $ \ptr -> do
        pokeByteOff (castPtr ptr) frameNbSamplesOffset (fromIntegral nsamples :: CInt)
        pokeByteOff (castPtr ptr) frameSampleRateOffset (fromIntegral rate :: CInt)
        pokeByteOff (castPtr ptr) frameFormatOffset fmt
        let chLayoutPtr = castPtr (ptr `plusPtr` frameChLayoutOffset)
        av_channel_layout_default chLayoutPtr (fromIntegral chans)

setFrameParamsWithLayout :: FrameHandle -> Int -> Int -> ChannelLayout -> AVSampleFormat -> IO ()
setFrameParamsWithLayout (FrameHandle fptr) nsamples rate layout fmt =
    withForeignPtr fptr $ \ptr -> do
        pokeByteOff (castPtr ptr) frameNbSamplesOffset (fromIntegral nsamples :: CInt)
        pokeByteOff (castPtr ptr) frameSampleRateOffset (fromIntegral rate :: CInt)
        pokeByteOff (castPtr ptr) frameFormatOffset fmt
        let chLayoutPtr = castPtr (ptr `plusPtr` frameChLayoutOffset)
        initChannelLayout chLayoutPtr layout

setVideoFrameParams :: FrameHandle -> Int -> Int -> AVPixelFormat -> IO ()
setVideoFrameParams (FrameHandle fptr) w h pixFmt =
    withForeignPtr fptr $ \ptr -> do
        pokeByteOff (castPtr ptr) frameWidthOffset (fromIntegral w :: CInt)
        pokeByteOff (castPtr ptr) frameHeightOffset (fromIntegral h :: CInt)
        pokeByteOff (castPtr ptr) frameFormatOffset pixFmt
        pokeByteOff (castPtr ptr) frameColorRangeOffset avColRangeMpeg
        pokeByteOff (castPtr ptr) frameColorPrimariesOffset avColPriBt709
        pokeByteOff (castPtr ptr) frameColorTrcOffset avColTrcBt709
        pokeByteOff (castPtr ptr) frameColorspaceOffset avColSpcBt709

getFrameBuffer :: FrameHandle -> IO ()
getFrameBuffer (FrameHandle fptr) =
    withForeignPtr fptr $ \ptr -> do
        ret <- c_av_frame_get_buffer ptr avFrameBufferAlign0
        _ <- errorNonNegative "av_frame_get_buffer" (fromIntegral ret)
        return ()

makeFrameWritable :: FrameHandle -> IO ()
makeFrameWritable (FrameHandle fptr) =
    withForeignPtr fptr $ \ptr -> do
        ret <- c_av_frame_make_writable ptr
        _ <- errorNonNegative "av_frame_make_writable" (fromIntegral ret)
        return ()

freeFrame :: FrameHandle -> IO ()
freeFrame (FrameHandle fptr) = finalizeForeignPtr fptr

withFrame :: Int -> Int -> Int -> AVSampleFormat -> (FrameHandle -> IO a) -> IO a
withFrame nsamples rate chans fmt action =
    bracket
        allocateFrame
        freeFrame
        (\h -> setFrameParams h nsamples rate chans fmt >> getFrameBuffer h >> action h)

withFrameWithLayout :: Int -> Int -> ChannelLayout -> AVSampleFormat -> (FrameHandle -> IO a) -> IO a
withFrameWithLayout nsamples rate layout fmt action =
    bracket
        allocateFrame
        freeFrame
        (\h -> setFrameParamsWithLayout h nsamples rate layout fmt >> getFrameBuffer h >> action h)

withVideoFrame :: Int -> Int -> AVPixelFormat -> (FrameHandle -> IO a) -> IO a
withVideoFrame w h pixFmt action =
    bracket
        allocateFrame
        freeFrame
        (\fh -> setVideoFrameParams fh w h pixFmt >> getFrameBuffer fh >> action fh)

withEmptyFrame :: (FrameHandle -> IO a) -> IO a
withEmptyFrame = bracket allocateFrame freeFrame

getFramePtr :: FrameHandle -> Ptr AVFrame
getFramePtr (FrameHandle fptr) = unsafeForeignPtrToPtr fptr
