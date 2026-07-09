module FFmpeg.Audio.Internal.Packet
    ( PacketHandle
    , allocatePacket
    , unrefPacket
    , freePacket
    , withPacket
    , getPacketPtr
    ) where

#include <libavcodec/packet.h>

import Control.Exception (bracket, throwIO)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr
    (ForeignPtr, finalizeForeignPtr, withForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (pokeByteOff)

import FFmpeg.Audio.Internal.Error (FFmpegError(..))
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Types

newtype PacketHandle = PacketHandle (ForeignPtr AVPacket)

allocatePacket :: IO PacketHandle
allocatePacket = do
    ptr <- c_av_packet_alloc
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "Failed to allocate AVPacket"
        else do
            let finalizer = alloca $ \pPtr -> do
                    pokeByteOff (castPtr pPtr) (0 :: Int) ptr
                    c_av_packet_free pPtr
            fptr <- newForeignPtr ptr finalizer
            return $ PacketHandle fptr

unrefPacket :: PacketHandle -> IO ()
unrefPacket (PacketHandle fptr) =
    withForeignPtr fptr c_av_packet_unref

freePacket :: PacketHandle -> IO ()
freePacket (PacketHandle fptr) = finalizeForeignPtr fptr

withPacket :: (PacketHandle -> IO a) -> IO a
withPacket action =
    bracket
        allocatePacket
        freePacket
        action

getPacketPtr :: PacketHandle -> Ptr AVPacket
getPacketPtr (PacketHandle fptr) = unsafeForeignPtrToPtr fptr