module FFmpeg.Audio.Internal.StreamGroup
    ( StreamGroupHandle
    , avStreamGroupParamsNone
    , avStreamGroupParamsIamfAudioElement
    , avStreamGroupParamsIamfMixPresentation
    , createStreamGroup
    , addStreamToGroup
    , setStreamGroupId
    , getStreamGroupId
    , getStreamGroupAudioElement
    , getStreamGroupMixPresentation
    , getStreamGroupPtr
    ) where

#include <libavformat/avformat.h>

import Control.Exception (throwIO)
import Data.Int (Int64)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Format
    ( FormatContextHandle
    , StreamHandle
    , getFormatContextPtr
    , getStreamPtr
    )
import FFmpeg.Audio.Internal.Types

newtype StreamGroupHandle = StreamGroupHandle (Ptr AVStreamGroup)

#{enum CInt,
 , avStreamGroupParamsNone                 = AV_STREAM_GROUP_PARAMS_NONE
 , avStreamGroupParamsIamfAudioElement     = AV_STREAM_GROUP_PARAMS_IAMF_AUDIO_ELEMENT
 , avStreamGroupParamsIamfMixPresentation  = AV_STREAM_GROUP_PARAMS_IAMF_MIX_PRESENTATION
 }

sgIdOffset :: Int
sgIdOffset = #{offset AVStreamGroup, id}

sgAudioElementOffset :: Int
sgAudioElementOffset = #{offset AVStreamGroup, params.iamf_audio_element}

sgMixPresentationOffset :: Int
sgMixPresentationOffset = #{offset AVStreamGroup, params.iamf_mix_presentation}

createStreamGroup :: FormatContextHandle -> CInt -> IO StreamGroupHandle
createStreamGroup fmt groupType = do
    ptr <- c_avformat_stream_group_create (getFormatContextPtr fmt) groupType nullPtr
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "avformat_stream_group_create returned null"
        else return $ StreamGroupHandle ptr

addStreamToGroup :: StreamGroupHandle -> StreamHandle -> IO ()
addStreamToGroup (StreamGroupHandle sgPtr) sh = do
    ret <- c_avformat_stream_group_add_stream sgPtr (getStreamPtr sh)
    _ <- errorNonNegative "avformat_stream_group_add_stream" (fromIntegral ret)
    return ()

setStreamGroupId :: StreamGroupHandle -> Int64 -> IO ()
setStreamGroupId (StreamGroupHandle sgPtr) gid =
    pokeByteOff (castPtr sgPtr) sgIdOffset gid

getStreamGroupId :: StreamGroupHandle -> IO Int64
getStreamGroupId (StreamGroupHandle sgPtr) =
    peekByteOff (castPtr sgPtr) sgIdOffset

getStreamGroupAudioElement :: StreamGroupHandle -> IO (Ptr AVIAMFAudioElement)
getStreamGroupAudioElement (StreamGroupHandle sgPtr) = do
    ptr <- peekByteOff (castPtr sgPtr) sgAudioElementOffset :: IO (Ptr AVIAMFAudioElement)
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "AVStreamGroup has null iamf_audio_element"
        else return ptr

getStreamGroupMixPresentation :: StreamGroupHandle -> IO (Ptr AVIAMFMixPresentation)
getStreamGroupMixPresentation (StreamGroupHandle sgPtr) = do
    ptr <- peekByteOff (castPtr sgPtr) sgMixPresentationOffset :: IO (Ptr AVIAMFMixPresentation)
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "AVStreamGroup has null iamf_mix_presentation"
        else return ptr

getStreamGroupPtr :: StreamGroupHandle -> Ptr AVStreamGroup
getStreamGroupPtr (StreamGroupHandle p) = p
