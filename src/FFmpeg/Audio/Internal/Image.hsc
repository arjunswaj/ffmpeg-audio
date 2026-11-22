module FFmpeg.Audio.Internal.Image
    ( Yuv420Image(..)
    , defaultDarkSlateImage
    , decodeCoverToYuv420
    , fillVideoFrame
    ) where

#include <libavutil/frame.h>

import Control.Exception (finally, throwIO)
import Control.Monad.ST (runST)
import Data.Bits ((.&.), shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word32, Word8)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, peekElemOff, sizeOf)

import FFmpeg.Audio.Internal.Codec (findDecoderById)
import FFmpeg.Audio.Internal.CodecContext (getCodecContextPtr, withDecoderContext)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Frame (FrameHandle, getFramePtr, withEmptyFrame)
import FFmpeg.Audio.Internal.Packet (getPacketPtr, unrefPacket, withPacket)
import FFmpeg.Audio.Internal.Types

data Yuv420Image = Yuv420Image
    { yuvWidth  :: !Int
    , yuvHeight :: !Int
    , yuvPlaneY :: !(VS.Vector Word8)
    , yuvPlaneU :: !(VS.Vector Word8)
    , yuvPlaneV :: !(VS.Vector Word8)
    } deriving (Show, Eq)

defaultDarkSlateImage :: Yuv420Image
defaultDarkSlateImage =
    let !w = 1280
        !h = 720
        !cw = w `shiftR` 1
        !ch = h `shiftR` 1
    in Yuv420Image
        { yuvWidth = w
        , yuvHeight = h
        , yuvPlaneY = VS.replicate (w * h) 37
        , yuvPlaneU = VS.replicate (cw * ch) 133
        , yuvPlaneV = VS.replicate (cw * ch) 125
        }

detectImageCodec :: BS.ByteString -> IO AVCodecID
detectImageCodec bs
    | BS.length bs >= 4
        && BS.index bs 0 == 0x89
        && BS.index bs 1 == 0x50
        && BS.index bs 2 == 0x4e
        && BS.index bs 3 == 0x47 = return avCodecIdPng
    | BS.length bs >= 2
        && BS.index bs 0 == 0xff
        && BS.index bs 1 == 0xd8 = return avCodecIdMjpeg
    | otherwise =
        throwIO $ FFmpegError (-1) "Unsupported cover image format (expected JPEG or PNG)"

decodeCoverToYuv420 :: BS.ByteString -> IO Yuv420Image
decodeCoverToYuv420 imgBytes = do
    codecId <- detectImageCodec imgBytes
    codec <- findDecoderById codecId
    withDecoderContext codec $ \ctx ->
        withPacket $ \ph ->
            withEmptyFrame $ \fh -> do
                let pktPtr = getPacketPtr ph
                    ctxPtr = getCodecContextPtr ctx
                    framePtr = getFramePtr fh
                    !len = BS.length imgBytes
                rAlloc <- c_av_new_packet pktPtr (fromIntegral len)
                _ <- errorNonNegative "av_new_packet(cover)" (fromIntegral rAlloc)
                finally
                    (do
                        dataPtr <- peekByteOff (castPtr pktPtr) packetDataOffset :: IO (Ptr Word8)
                        BSU.unsafeUseAsCString imgBytes $ \srcPtr ->
                            copyBytes dataPtr (castPtr srcPtr) len
                        rSend <- c_avcodec_send_packet ctxPtr pktPtr
                        _ <- errorNonNegative "avcodec_send_packet(cover)" (fromIntegral rSend)
                        rRecv <- c_avcodec_receive_frame ctxPtr framePtr
                        if rRecv == averrorEagain
                            then do
                                rFlush <- c_avcodec_send_packet ctxPtr nullPtr
                                _ <- errorNonNegative "avcodec_send_packet(cover flush)" (fromIntegral rFlush)
                                rRecv2 <- c_avcodec_receive_frame ctxPtr framePtr
                                _ <- errorNonNegative "avcodec_receive_frame(cover flush)" (fromIntegral rRecv2)
                                finally (frameToYuv420 framePtr) (c_av_frame_unref framePtr)
                            else do
                                _ <- errorNonNegative "avcodec_receive_frame(cover)" (fromIntegral rRecv)
                                finally (frameToYuv420 framePtr) (c_av_frame_unref framePtr))
                    (unrefPacket ph)

ptrSize :: Int
ptrSize = sizeOf (undefined :: Ptr ())

cintSize :: Int
cintSize = sizeOf (undefined :: CInt)

readPlanePtr :: Ptr AVFrame -> Int -> IO (Ptr Word8)
readPlanePtr framePtr idx =
    peekByteOff (castPtr framePtr) (frameDataArrayOffset + idx * ptrSize)

readLinesize :: Ptr AVFrame -> Int -> IO Int
readLinesize framePtr idx = do
    ls <- peekByteOff (castPtr framePtr) (frameLinesizeOffset + idx * cintSize) :: IO CInt
    return (fromIntegral ls)

frameToYuv420 :: Ptr AVFrame -> IO Yuv420Image
frameToYuv420 framePtr = do
    wC <- peekByteOff (castPtr framePtr) frameWidthOffset :: IO CInt
    hC <- peekByteOff (castPtr framePtr) frameHeightOffset :: IO CInt
    fmt <- peekByteOff (castPtr framePtr) frameFormatOffset :: IO AVPixelFormat
    let !srcW = fromIntegral wC
        !srcH = fromIntegral hC
    if srcW <= 0 || srcH <= 0
        then throwIO $ FFmpegError (-1) "Decoded cover image has invalid dimensions"
        else do
            rgb <- extractRgb24 framePtr srcW srcH fmt
            let (!dstW, !dstH) = computeTargetDims srcW srcH
                !resized =
                    if dstW == srcW && dstH == srcH
                        then rgb
                        else resizeRgb24Bilinear srcW srcH rgb dstW dstH
            return $! rgb24ToYuv420Bt709 dstW dstH resized

computeTargetDims :: Int -> Int -> (Int, Int)
computeTargetDims w h =
    let !maxDim = 1920.0 :: Double
        !scale =
            if max w h > 1920
                then maxDim / fromIntegral (max w h)
                else 1.0
        !scaledW = round (fromIntegral w * scale) :: Int
        !scaledH = round (fromIntegral h * scale) :: Int
        evenDim !d = max 16 (((d + 1) `shiftR` 1) * 2)
    in (evenDim scaledW, evenDim scaledH)

clampByte :: Int -> Word8
clampByte !x
    | x <= 0 = 0
    | x >= 255 = 255
    | otherwise = fromIntegral x
{-# INLINE clampByte #-}

clampRange :: Int -> Int -> Int -> Word8
clampRange !lo !hi !x
    | x <= lo = fromIntegral lo
    | x >= hi = fromIntegral hi
    | otherwise = fromIntegral x
{-# INLINE clampRange #-}

extractRgb24 :: Ptr AVFrame -> Int -> Int -> AVPixelFormat -> IO (VS.Vector Word8)
extractRgb24 framePtr w h fmt
    | fmt == avPixFmtRgb24 = extractPacked framePtr w h 3 False
    | fmt == avPixFmtBgr24 = extractPacked framePtr w h 3 True
    | fmt == avPixFmtRgba = extractRgba framePtr w h False
    | fmt == avPixFmtBgra = extractRgba framePtr w h True
    | fmt == avPixFmtGray8 = extractGray framePtr w h 1
    | fmt == avPixFmtYa8 = extractGray framePtr w h 2
    | fmt == avPixFmtRgb48be = extractRgb16Be framePtr w h 6
    | fmt == avPixFmtRgba64be = extractRgb16Be framePtr w h 8
    | fmt == avPixFmtPal8 = extractPal8 framePtr w h
    | fmt == avPixFmtYuv420p = extractYuv framePtr w h 1 1 False
    | fmt == avPixFmtYuvj420p = extractYuv framePtr w h 1 1 True
    | fmt == avPixFmtYuv422p = extractYuv framePtr w h 1 0 False
    | fmt == avPixFmtYuvj422p = extractYuv framePtr w h 1 0 True
    | fmt == avPixFmtYuv444p = extractYuv framePtr w h 0 0 False
    | fmt == avPixFmtYuvj444p = extractYuv framePtr w h 0 0 True
    | otherwise = throwIO $ FFmpegError (-1) ("Unsupported cover image pixel format: " ++ show fmt)

extractPacked :: Ptr AVFrame -> Int -> Int -> Int -> Bool -> IO (VS.Vector Word8)
extractPacked framePtr !w !h !bpp !swapRb = do
    p0 <- readPlanePtr framePtr 0
    !ls0 <- readLinesize framePtr 0
    mv <- VSM.new (w * h * 3)
    let goY !y
            | y >= h = return ()
            | otherwise = do
                let !rowPtr = p0 `plusPtr` (y * ls0) :: Ptr Word8
                    !rowOff = y * w * 3
                    goX !x !srcOff !dstOff
                        | x >= w = return ()
                        | otherwise = do
                            !c0 <- peekElemOff rowPtr srcOff
                            !c1 <- peekElemOff rowPtr (srcOff + 1)
                            !c2 <- peekElemOff rowPtr (srcOff + 2)
                            if swapRb
                                then do
                                    VSM.unsafeWrite mv dstOff c2
                                    VSM.unsafeWrite mv (dstOff + 1) c1
                                    VSM.unsafeWrite mv (dstOff + 2) c0
                                else do
                                    VSM.unsafeWrite mv dstOff c0
                                    VSM.unsafeWrite mv (dstOff + 1) c1
                                    VSM.unsafeWrite mv (dstOff + 2) c2
                            goX (x + 1) (srcOff + bpp) (dstOff + 3)
                goX 0 0 rowOff
                goY (y + 1)
    goY 0
    VS.unsafeFreeze mv

extractRgba :: Ptr AVFrame -> Int -> Int -> Bool -> IO (VS.Vector Word8)
extractRgba framePtr !w !h !isBgra = do
    p0 <- readPlanePtr framePtr 0
    !ls0 <- readLinesize framePtr 0
    mv <- VSM.new (w * h * 3)
    let blend !fg !bg !ai = clampByte ((fromIntegral fg * ai + bg * (255 - ai) + 127) `div` 255)
        goY !y
            | y >= h = return ()
            | otherwise = do
                let !rowPtr = p0 `plusPtr` (y * ls0) :: Ptr Word8
                    !rowOff = y * w * 3
                    goX !x !srcOff !dstOff
                        | x >= w = return ()
                        | otherwise = do
                            !c0 <- peekElemOff rowPtr srcOff
                            !c1 <- peekElemOff rowPtr (srcOff + 1)
                            !c2 <- peekElemOff rowPtr (srcOff + 2)
                            !a <- peekElemOff rowPtr (srcOff + 3)
                            let !ai = fromIntegral a :: Int
                                !r0 = if isBgra then c2 else c0
                                !b0 = if isBgra then c0 else c2
                            if ai == 255
                                then do
                                    VSM.unsafeWrite mv dstOff r0
                                    VSM.unsafeWrite mv (dstOff + 1) c1
                                    VSM.unsafeWrite mv (dstOff + 2) b0
                                else do
                                    VSM.unsafeWrite mv dstOff (blend r0 22 ai)
                                    VSM.unsafeWrite mv (dstOff + 1) (blend c1 27 ai)
                                    VSM.unsafeWrite mv (dstOff + 2) (blend b0 38 ai)
                            goX (x + 1) (srcOff + 4) (dstOff + 3)
                goX 0 0 rowOff
                goY (y + 1)
    goY 0
    VS.unsafeFreeze mv

extractGray :: Ptr AVFrame -> Int -> Int -> Int -> IO (VS.Vector Word8)
extractGray framePtr !w !h !bpp = do
    p0 <- readPlanePtr framePtr 0
    !ls0 <- readLinesize framePtr 0
    mv <- VSM.new (w * h * 3)
    let goY !y
            | y >= h = return ()
            | otherwise = do
                let !rowPtr = p0 `plusPtr` (y * ls0) :: Ptr Word8
                    !rowOff = y * w * 3
                    goX !x !srcOff !dstOff
                        | x >= w = return ()
                        | otherwise = do
                            !v <- peekElemOff rowPtr srcOff
                            VSM.unsafeWrite mv dstOff v
                            VSM.unsafeWrite mv (dstOff + 1) v
                            VSM.unsafeWrite mv (dstOff + 2) v
                            goX (x + 1) (srcOff + bpp) (dstOff + 3)
                goX 0 0 rowOff
                goY (y + 1)
    goY 0
    VS.unsafeFreeze mv

extractRgb16Be :: Ptr AVFrame -> Int -> Int -> Int -> IO (VS.Vector Word8)
extractRgb16Be framePtr !w !h !bpp = do
    p0 <- readPlanePtr framePtr 0
    !ls0 <- readLinesize framePtr 0
    mv <- VSM.new (w * h * 3)
    let goY !y
            | y >= h = return ()
            | otherwise = do
                let !rowPtr = p0 `plusPtr` (y * ls0) :: Ptr Word8
                    !rowOff = y * w * 3
                    goX !x !srcOff !dstOff
                        | x >= w = return ()
                        | otherwise = do
                            !r <- peekElemOff rowPtr srcOff
                            !g <- peekElemOff rowPtr (srcOff + 2)
                            !b <- peekElemOff rowPtr (srcOff + 4)
                            VSM.unsafeWrite mv dstOff r
                            VSM.unsafeWrite mv (dstOff + 1) g
                            VSM.unsafeWrite mv (dstOff + 2) b
                            goX (x + 1) (srcOff + bpp) (dstOff + 3)
                goX 0 0 rowOff
                goY (y + 1)
    goY 0
    VS.unsafeFreeze mv

extractPal8 :: Ptr AVFrame -> Int -> Int -> IO (VS.Vector Word8)
extractPal8 framePtr !w !h = do
    p0 <- readPlanePtr framePtr 0
    palRaw <- readPlanePtr framePtr 1
    !ls0 <- readLinesize framePtr 0
    let !palPtr = castPtr palRaw :: Ptr Word32
    mv <- VSM.new (w * h * 3)
    let goY !y
            | y >= h = return ()
            | otherwise = do
                let !rowPtr = p0 `plusPtr` (y * ls0) :: Ptr Word8
                    !rowOff = y * w * 3
                    goX !x !dstOff
                        | x >= w = return ()
                        | otherwise = do
                            !idx <- peekElemOff rowPtr x
                            !argb <- peekElemOff palPtr (fromIntegral idx)
                            VSM.unsafeWrite mv dstOff (fromIntegral ((argb `shiftR` 16) .&. 0xff))
                            VSM.unsafeWrite mv (dstOff + 1) (fromIntegral ((argb `shiftR` 8) .&. 0xff))
                            VSM.unsafeWrite mv (dstOff + 2) (fromIntegral (argb .&. 0xff))
                            goX (x + 1) (dstOff + 3)
                goX 0 rowOff
                goY (y + 1)
    goY 0
    VS.unsafeFreeze mv

extractYuv :: Ptr AVFrame -> Int -> Int -> Int -> Int -> Bool -> IO (VS.Vector Word8)
extractYuv framePtr !w !h !shiftX !shiftY !fullRange = do
    pY <- readPlanePtr framePtr 0
    pU <- readPlanePtr framePtr 1
    pV <- readPlanePtr framePtr 2
    !lsY <- readLinesize framePtr 0
    !lsU <- readLinesize framePtr 1
    !lsV <- readLinesize framePtr 2
    mv <- VSM.new (w * h * 3)
    let goY !y
            | y >= h = return ()
            | otherwise = do
                let !cy = y `shiftR` shiftY
                    !rowY = pY `plusPtr` (y * lsY) :: Ptr Word8
                    !rowU = pU `plusPtr` (cy * lsU) :: Ptr Word8
                    !rowV = pV `plusPtr` (cy * lsV) :: Ptr Word8
                    !rowOff = y * w * 3
                    goX !x !dstOff
                        | x >= w = return ()
                        | otherwise = do
                            let !cx = x `shiftR` shiftX
                            !yv <- peekElemOff rowY x
                            !uv <- peekElemOff rowU cx
                            !vv <- peekElemOff rowV cx
                            let !yi = fromIntegral yv :: Int
                                !ui = fromIntegral uv - 128 :: Int
                                !vi = fromIntegral vv - 128 :: Int
                            if fullRange
                                then do
                                    let !r = yi + ((91881 * vi + 32768) `shiftR` 16)
                                        !g = yi - ((22554 * ui + 46802 * vi + 32768) `shiftR` 16)
                                        !b = yi + ((116130 * ui + 32768) `shiftR` 16)
                                    VSM.unsafeWrite mv dstOff (clampByte r)
                                    VSM.unsafeWrite mv (dstOff + 1) (clampByte g)
                                    VSM.unsafeWrite mv (dstOff + 2) (clampByte b)
                                else do
                                    let !ys = 76309 * (yi - 16)
                                        !r = (ys + 104597 * vi + 32768) `shiftR` 16
                                        !g = (ys - 25675 * ui - 53279 * vi + 32768) `shiftR` 16
                                        !b = (ys + 132201 * ui + 32768) `shiftR` 16
                                    VSM.unsafeWrite mv dstOff (clampByte r)
                                    VSM.unsafeWrite mv (dstOff + 1) (clampByte g)
                                    VSM.unsafeWrite mv (dstOff + 2) (clampByte b)
                            goX (x + 1) (dstOff + 3)
                goX 0 rowOff
                goY (y + 1)
    goY 0
    VS.unsafeFreeze mv

resizeRgb24Bilinear :: Int -> Int -> VS.Vector Word8 -> Int -> Int -> VS.Vector Word8
resizeRgb24Bilinear !srcW !srcH src !dstW !dstH = runST $ do
    mv <- VSM.new (dstW * dstH * 3)
    let !scaleX = fromIntegral srcW / fromIntegral dstW :: Double
        !scaleY = fromIntegral srcH / fromIntegral dstH :: Double
        !maxX = srcW - 1
        !maxY = srcH - 1
        !srcStride = srcW * 3
        goY !dy
            | dy >= dstH = return ()
            | otherwise = do
                let !sy = (fromIntegral dy + 0.5) * scaleY - 0.5
                    !y0 = max 0 (min maxY (floor sy))
                    !y1 = min maxY (y0 + 1)
                    !fy = round (max 0.0 (min 1.0 (sy - fromIntegral y0)) * 256.0) :: Int
                    !ify = 256 - fy
                    !row0 = y0 * srcStride
                    !row1 = y1 * srcStride
                    !dstRow = dy * dstW * 3
                    goX !dx !dstOff
                        | dx >= dstW = return ()
                        | otherwise = do
                            let !sx = (fromIntegral dx + 0.5) * scaleX - 0.5
                                !x0 = max 0 (min maxX (floor sx))
                                !x1 = min maxX (x0 + 1)
                                !fx = round (max 0.0 (min 1.0 (sx - fromIntegral x0)) * 256.0) :: Int
                                !ifx = 256 - fx
                                !p00 = row0 + x0 * 3
                                !p10 = row0 + x1 * 3
                                !p01 = row1 + x0 * 3
                                !p11 = row1 + x1 * 3
                                interp !c =
                                    let !v00 = fromIntegral (VS.unsafeIndex src (p00 + c)) :: Int
                                        !v10 = fromIntegral (VS.unsafeIndex src (p10 + c)) :: Int
                                        !v01 = fromIntegral (VS.unsafeIndex src (p01 + c)) :: Int
                                        !v11 = fromIntegral (VS.unsafeIndex src (p11 + c)) :: Int
                                        !top = v00 * ifx + v10 * fx
                                        !bot = v01 * ifx + v11 * fx
                                    in clampByte ((top * ify + bot * fy + 32768) `shiftR` 16)
                            VSM.unsafeWrite mv dstOff (interp 0)
                            VSM.unsafeWrite mv (dstOff + 1) (interp 1)
                            VSM.unsafeWrite mv (dstOff + 2) (interp 2)
                            goX (dx + 1) (dstOff + 3)
                goX 0 dstRow
                goY (dy + 1)
    goY 0
    VS.unsafeFreeze mv

rgb24ToYuv420Bt709 :: Int -> Int -> VS.Vector Word8 -> Yuv420Image
rgb24ToYuv420Bt709 !w !h rgb = runST $ do
    let !cw = w `shiftR` 1
        !ch = h `shiftR` 1
        !rgbStride = w * 3
    mY <- VSM.new (w * h)
    mU <- VSM.new (cw * ch)
    mV <- VSM.new (cw * ch)
    let toY !r !g !b = clampRange 16 235 (((11966 * r + 40254 * g + 4064 * b + 32768) `shiftR` 16) + 16)
        goCy !cy
            | cy >= ch = return ()
            | otherwise = do
                let !y0 = cy * 2
                    !y1 = y0 + 1
                    !srcRow0 = y0 * rgbStride
                    !srcRow1 = y1 * rgbStride
                    !yDstRow0 = y0 * w
                    !yDstRow1 = y1 * w
                    !uvRow = cy * cw
                    goCx !cx
                        | cx >= cw = return ()
                        | otherwise = do
                            let !x0 = cx * 2
                                !x1 = x0 + 1
                                !i00 = srcRow0 + x0 * 3
                                !i10 = srcRow0 + x1 * 3
                                !i01 = srcRow1 + x0 * 3
                                !i11 = srcRow1 + x1 * 3
                                !r00 = fromIntegral (VS.unsafeIndex rgb i00) :: Int
                                !g00 = fromIntegral (VS.unsafeIndex rgb (i00 + 1)) :: Int
                                !b00 = fromIntegral (VS.unsafeIndex rgb (i00 + 2)) :: Int
                                !r10 = fromIntegral (VS.unsafeIndex rgb i10) :: Int
                                !g10 = fromIntegral (VS.unsafeIndex rgb (i10 + 1)) :: Int
                                !b10 = fromIntegral (VS.unsafeIndex rgb (i10 + 2)) :: Int
                                !r01 = fromIntegral (VS.unsafeIndex rgb i01) :: Int
                                !g01 = fromIntegral (VS.unsafeIndex rgb (i01 + 1)) :: Int
                                !b01 = fromIntegral (VS.unsafeIndex rgb (i01 + 2)) :: Int
                                !r11 = fromIntegral (VS.unsafeIndex rgb i11) :: Int
                                !g11 = fromIntegral (VS.unsafeIndex rgb (i11 + 1)) :: Int
                                !b11 = fromIntegral (VS.unsafeIndex rgb (i11 + 2)) :: Int
                                !rSum = r00 + r10 + r01 + r11
                                !gSum = g00 + g10 + g01 + g11
                                !bSum = b00 + b10 + b01 + b11
                                !uVal = clampRange 16 240 (((-6596 * rSum - 22189 * gSum + 28785 * bSum + 131072) `shiftR` 18) + 128)
                                !vVal = clampRange 16 240 (((28785 * rSum - 26145 * gSum - 2639 * bSum + 131072) `shiftR` 18) + 128)
                                !uvIdx = uvRow + cx
                            VSM.unsafeWrite mY (yDstRow0 + x0) (toY r00 g00 b00)
                            VSM.unsafeWrite mY (yDstRow0 + x1) (toY r10 g10 b10)
                            VSM.unsafeWrite mY (yDstRow1 + x0) (toY r01 g01 b01)
                            VSM.unsafeWrite mY (yDstRow1 + x1) (toY r11 g11 b11)
                            VSM.unsafeWrite mU uvIdx uVal
                            VSM.unsafeWrite mV uvIdx vVal
                            goCx (cx + 1)
                goCx 0
                goCy (cy + 1)
    goCy 0
    !pY <- VS.unsafeFreeze mY
    !pU <- VS.unsafeFreeze mU
    !pV <- VS.unsafeFreeze mV
    return Yuv420Image
        { yuvWidth = w
        , yuvHeight = h
        , yuvPlaneY = pY
        , yuvPlaneU = pU
        , yuvPlaneV = pV
        }

fillVideoFrame :: FrameHandle -> Yuv420Image -> IO ()
fillVideoFrame fh img = do
    let framePtr = getFramePtr fh
        !w = yuvWidth img
        !h = yuvHeight img
        !cw = w `shiftR` 1
        !ch = h `shiftR` 1
    pY <- readPlanePtr framePtr 0
    pU <- readPlanePtr framePtr 1
    pV <- readPlanePtr framePtr 2
    !lsY <- readLinesize framePtr 0
    !lsU <- readLinesize framePtr 1
    !lsV <- readLinesize framePtr 2
    copyPlane pY lsY w h (yuvPlaneY img)
    copyPlane pU lsU cw ch (yuvPlaneU img)
    copyPlane pV lsV cw ch (yuvPlaneV img)
  where
    copyPlane dstPtr !ls !rowW !rows vec =
        VS.unsafeWith vec $ \srcPtr ->
            let go !r
                    | r >= rows = return ()
                    | otherwise = do
                        copyBytes (dstPtr `plusPtr` (r * ls)) (srcPtr `plusPtr` (r * rowW)) rowW
                        go (r + 1)
            in go 0
