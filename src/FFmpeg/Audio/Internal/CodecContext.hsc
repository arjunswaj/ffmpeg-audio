module FFmpeg.Audio.Internal.CodecContext
    ( CodecContextHandle
    , CodecContextParams(..)
    , allocateCodecContext
    , configureCodecContext
    , configureCodecContextWith
    , openCodecContext
    , freeCodecContext
    , withCodecContext
    , withCodecContextWith
    , getCodecContextPtr
    , getCodecFrameSize
    , getCodecTimeBase
    , initChannelLayout
    ) where

#include <libavcodec/avcodec.h>
#include <libavutil/channel_layout.h>
#include <libavutil/samplefmt.h>

import Control.Exception (bracket, throwIO)
import Control.Monad (forM_, when)
import Data.Bits ((.|.))
import Data.Int (Int64)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr
    (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable, peekByteOff, pokeByteOff)

import FFmpeg.Audio.Internal.Codec (CodecHandle, getCodecPtr)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Types
import FFmpeg.Audio.PCMBuffer (ChannelLayout(..))

data CodecContextHandle = CodecContextHandle CodecHandle (ForeignPtr AVCodecContext)

data CodecContextParams = CodecContextParams
    { ccSampleRate       :: !Int
    , ccChannelLayout    :: !ChannelLayout
    , ccSampleFormat     :: !AVSampleFormat
    , ccBitRate          :: !(Maybe Int)
    , ccBitsPerRawSample :: !(Maybe Int)
    , ccGlobalHeader     :: !Bool
    , ccIntOptions       :: ![(String, Int64)]
    } deriving (Show, Eq)

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

frameSizeOffset :: Int
frameSizeOffset = #{offset AVCodecContext, frame_size}

bitsPerRawSampleOffset :: Int
bitsPerRawSampleOffset = #{offset AVCodecContext, bits_per_raw_sample}

timeBaseOffset :: Int
timeBaseOffset = #{offset AVCodecContext, time_base}

flagsOffset :: Int
flagsOffset = #{offset AVCodecContext, flags}

setField :: Storable a => Ptr AVCodecContext -> Int -> a -> IO ()
setField ptr offset val = pokeByteOff (castPtr ptr) offset val

initChannelLayout :: Ptr AVChannelLayout -> ChannelLayout -> IO ()
initChannelLayout chLayoutPtr layout = do
    c_av_channel_layout_uninit chLayoutPtr
    case layout of
        LayoutMono -> setMask avChannelLayoutMono
        LayoutStereo -> setMask avChannelLayoutStereo
        Layout5_1 -> setMask avChannelLayout5Point1
        Layout7_1 -> setMask avChannelLayout7Point1
        Layout7_1_2 -> setMask avChannelLayout7Point1Point2
        Layout7_1_4 -> setMask avChannelLayout7Point1Point4Back
        LayoutBinaural -> setMask avChannelLayoutBinaural
        LayoutAmbisonic order ->
            withCString ("ambisonic " ++ show order) $ \cStr -> do
                ret <- c_av_channel_layout_from_string chLayoutPtr cStr
                _ <- errorNonNegative "av_channel_layout_from_string" (fromIntegral ret)
                return ()
        LayoutDefault n ->
            av_channel_layout_default chLayoutPtr (fromIntegral n)
  where
    setMask mask = do
        ret <- c_av_channel_layout_from_mask chLayoutPtr mask
        _ <- errorNonNegative "av_channel_layout_from_mask" (fromIntegral ret)
        return ()

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
        setField ptr bitRateOffset (320000 :: Int64)
        setField ptr strictStdComplianceOffset (-2 :: CInt)
        let chLayoutPtr = castPtr (ptr `plusPtr` chLayoutOffset)
        av_channel_layout_default chLayoutPtr (fromIntegral chans)
  where
    CodecContextHandle _ fptr = h

configureCodecContextWith :: CodecContextHandle -> CodecContextParams -> IO ()
configureCodecContextWith h params =
    withForeignPtr fptr $ \ptr -> do
        setField ptr sampleRateOffset (fromIntegral (ccSampleRate params) :: CInt)
        setField ptr sampleFmtOffset (ccSampleFormat params)
        setField ptr timeBaseOffset (AVRational 1 (fromIntegral (ccSampleRate params)))
        setField ptr strictStdComplianceOffset (-2 :: CInt)
        case ccBitRate params of
            Just br -> setField ptr bitRateOffset (fromIntegral br :: Int64)
            Nothing -> return ()
        case ccBitsPerRawSample params of
            Just bprs -> setField ptr bitsPerRawSampleOffset (fromIntegral bprs :: CInt)
            Nothing -> return ()
        when (ccGlobalHeader params) $ do
            curFlags <- peekByteOff (castPtr ptr) flagsOffset :: IO CInt
            setField ptr flagsOffset (curFlags .|. avCodecFlagGlobalHeader)
        let chLayoutPtr = castPtr (ptr `plusPtr` chLayoutOffset)
        initChannelLayout chLayoutPtr (ccChannelLayout params)
        forM_ (ccIntOptions params) $ \(k, v) ->
            withCString k $ \cKey -> do
                ret <- c_av_opt_set_int (castPtr ptr) cKey v avOptSearchChildren
                _ <- errorNonNegative ("av_opt_set_int(" ++ k ++ ")") (fromIntegral ret)
                return ()
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

withCodecContextWith :: CodecHandle -> CodecContextParams -> (CodecContextHandle -> IO a) -> IO a
withCodecContextWith codec params action =
    bracket
        (allocateCodecContext codec)
        freeCodecContext
        (\h -> configureCodecContextWith h params >> openCodecContext h >> action h)

getCodecContextPtr :: CodecContextHandle -> Ptr AVCodecContext
getCodecContextPtr (CodecContextHandle _ fptr) =
    unsafeForeignPtrToPtr fptr

getCodecFrameSize :: CodecContextHandle -> IO Int
getCodecFrameSize (CodecContextHandle _ fptr) =
    withForeignPtr fptr $ \ptr -> do
        fs <- peekByteOff (castPtr ptr) frameSizeOffset :: IO CInt
        return (fromIntegral fs)

getCodecTimeBase :: CodecContextHandle -> IO AVRational
getCodecTimeBase (CodecContextHandle _ fptr) =
    withForeignPtr fptr $ \ptr ->
        peekByteOff (castPtr ptr) timeBaseOffset
