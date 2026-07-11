module FFmpeg.Audio.Internal.Encoder
    ( encodePcmToFile
    ) where

import Control.Monad (when)
import Data.Int (Int16, Int64)
import Data.Vector qualified as V
import Foreign.C.Types (CUChar)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekByteOff, pokeByteOff, pokeElemOff, sizeOf)
import FFmpeg.Audio.Internal.Codec (findMp3Encoder)
import FFmpeg.Audio.Internal.CodecContext (CodecContextHandle, getCodecContextPtr, withCodecContext)
import FFmpeg.Audio.Internal.Error (errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Format (FormatContextHandle, withOutputFile, writeFrame)
import FFmpeg.Audio.Internal.Frame (FrameHandle, getFramePtr, withFrame, makeFrameWritable)
import FFmpeg.Audio.Internal.Packet (withPacket, unrefPacket, getPacketPtr)
import FFmpeg.Audio.Internal.Types (AVFrame, avSampleFormatS16P, averrorEagain, averrorEof, frameDataArrayOffset, framePtsOffset)
import FFmpeg.Audio.PCMBuffer (PCMBuffer(..))

mp3FrameSize :: Int
mp3FrameSize = 1152

ptrSize :: Int
ptrSize = sizeOf (undefined :: Ptr ())

encodePcmToFile :: FilePath -> PCMBuffer -> IO ()
encodePcmToFile path buf = do
    let sampleRate = pcmSampleRate buf
        channels = pcmChannels buf
        samples = pcmSamples buf
        totalSamples = V.length samples `div` channels
        fs = mp3FrameSize

    codec <- findMp3Encoder
    withCodecContext codec sampleRate channels $ \ctx ->
        withOutputFile path codec ctx $ \fmt -> do
            let numFullFrames = totalSamples `div` fs
            mapM_ (sendFrameData ctx fmt samples channels sampleRate fs) [0 .. numFullFrames - 1]

            let remaining = totalSamples `mod` fs
            when (remaining > 0) $
                sendPartialFrame ctx fmt samples channels sampleRate fs
                    (numFullFrames * fs) remaining

            flushEncoder ctx fmt

deinterleaveToFrame :: Ptr AVFrame -> V.Vector Int16 -> Int -> Int -> Int -> IO ()
deinterleaveToFrame frame samples channels nbSamples startOffset = do
    let go c = do
            let planeOffset = frameDataArrayOffset + c * ptrSize
            planePtr <- peekByteOff (castPtr frame) planeOffset :: IO (Ptr CUChar)
            let int16Plane = castPtr planePtr :: Ptr Int16
            let baseIdx = startOffset + c
            sequence_ [pokeElemOff int16Plane i (V.unsafeIndex samples (baseIdx + i * channels))
                      | i <- [0 .. nbSamples - 1]]
    mapM_ go [0 .. channels - 1]

sendFrameData :: CodecContextHandle -> FormatContextHandle -> V.Vector Int16 -> Int -> Int -> Int -> Int -> IO ()
sendFrameData ctx fmt samples channels sampleRate frameSize frameIdx =
    withFrame frameSize sampleRate channels avSampleFormatS16P $ \fh -> do
        makeFrameWritable fh
        let offset = frameIdx * frameSize * channels
        deinterleaveToFrame (getFramePtr fh) samples channels frameSize offset
        pokeByteOff (castPtr (getFramePtr fh)) framePtsOffset (fromIntegral (frameIdx * frameSize) :: Int64)
        doSendAndReceive ctx fmt fh

sendPartialFrame :: CodecContextHandle -> FormatContextHandle -> V.Vector Int16 -> Int -> Int -> Int -> Int -> Int -> IO ()
sendPartialFrame ctx fmt samples channels sampleRate _frameSize startOffset remaining =
    withFrame remaining sampleRate channels avSampleFormatS16P $ \fh -> do
        makeFrameWritable fh
        deinterleaveToFrame (getFramePtr fh) samples channels remaining startOffset
        pokeByteOff (castPtr (getFramePtr fh)) framePtsOffset (fromIntegral startOffset :: Int64)
        doSendAndReceive ctx fmt fh

doSendAndReceive :: CodecContextHandle -> FormatContextHandle -> FrameHandle -> IO ()
doSendAndReceive ctx fmt fh = do
    let ctxPtr = getCodecContextPtr ctx
    ret <- c_avcodec_send_frame ctxPtr (getFramePtr fh)
    _ <- errorNonNegative "avcodec_send_frame" (fromIntegral ret)
    receivePackets ctx fmt

receivePackets :: CodecContextHandle -> FormatContextHandle -> IO ()
receivePackets ctx fmt = do
    let ctxPtr = getCodecContextPtr ctx
    withPacket $ \ph -> do
        let loop = do
                ret <- c_avcodec_receive_packet ctxPtr (getPacketPtr ph)
                if ret == 0
                    then do
                        writeFrame fmt ph
                        unrefPacket ph
                        loop
                    else if ret == averrorEagain
                        then return ()
                        else errorNonNegative "avcodec_receive_packet" (fromIntegral ret) >> return ()
        loop

flushEncoder :: CodecContextHandle -> FormatContextHandle -> IO ()
flushEncoder ctx fmt = do
    let ctxPtr = getCodecContextPtr ctx
    ret <- c_avcodec_send_frame ctxPtr nullPtr
    _ <- errorNonNegative "avcodec_send_frame (flush)" (fromIntegral ret)
    withPacket $ \ph -> do
        let loop = do
                ret' <- c_avcodec_receive_packet ctxPtr (getPacketPtr ph)
                if ret' == averrorEof
                    then return ()
                    else do
                        _ <- errorNonNegative "avcodec_receive_packet (flush)" (fromIntegral ret')
                        writeFrame fmt ph
                        unrefPacket ph
                        loop
        loop
