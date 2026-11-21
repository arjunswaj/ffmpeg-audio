module FFmpeg.Audio.Internal.Iamf
    ( IamfAudioElementHandle
    , IamfMixPresentationHandle
    , IamfParamDefinitionHandle
    , avIamfAudioElementTypeChannel
    , avIamfAudioElementTypeScene
    , avIamfAmbisonicsModeMono
    , avIamfAmbisonicsModeProjection
    , avIamfParamDefMixGain
    , avIamfParamDefDemixing
    , avIamfParamDefReconGain
    , avIamfAnimationTypeStep
    , avIamfAnimationTypeLinear
    , avIamfAnimationTypeBezier
    , avIamfHeadphonesModeStereo
    , avIamfHeadphonesModeBinaural
    , avIamfSubmixLayoutTypeLoudspeakers
    , avIamfSubmixLayoutTypeBinaural
    , avIamfLayerFlagReconGain
    , allocateIamfAudioElement
    , freeIamfAudioElement
    , withIamfAudioElement
    , getIamfAudioElementPtr
    , allocateIamfMixPresentation
    , freeIamfMixPresentation
    , withIamfMixPresentation
    , getIamfMixPresentationPtr
    , allocateIamfParamDefinition
    , freeIamfParamDefinition
    , withIamfParamDefinition
    , getIamfParamDefinitionPtr
    , iamfAudioElementAddLayer
    , iamfAudioElementSetType
    , iamfAudioElementSetDefaultW
    , iamfAudioElementNbLayers
    , iamfLayerSetChannelLayout
    , iamfLayerSetAmbisonicsMode
    , iamfLayerSetFlags
    , iamfMixPresentationAddSubmix
    , iamfMixPresentationSetAnnotation
    , iamfMixPresentationNbSubmixes
    , iamfSubmixAddElement
    , iamfSubmixAddLayout
    , iamfSubmixSetDefaultMixGain
    , iamfSubmixSetOutputMixConfig
    , iamfSubmixNbElements
    , iamfSubmixNbLayouts
    , iamfSubmixElementSetAudioElementId
    , iamfSubmixElementSetDefaultMixGain
    , iamfSubmixElementSetHeadphonesMode
    , iamfSubmixElementSetAnnotation
    , iamfSubmixElementSetMixConfig
    , iamfSubmixLayoutSetType
    , iamfSubmixLayoutSetSoundSystem
    , iamfSubmixLayoutSetLoudness
    , iamfParamDefinitionGetSubblock
    , allocRawMixGainParam
    ) where

#include <libavutil/iamf.h>

import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import Foreign.C.String (withCString)
import Foreign.C.Types (CInt, CSize, CUInt)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, finalizeForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)

import FFmpeg.Audio.Internal.CodecContext (initChannelLayout)
import FFmpeg.Audio.Internal.Error (FFmpegError(..), errorNonNegative)
import FFmpeg.Audio.Internal.FFI
import FFmpeg.Audio.Internal.Types
import FFmpeg.Audio.PCMBuffer (ChannelLayout)

newtype IamfAudioElementHandle = IamfAudioElementHandle (ForeignPtr AVIAMFAudioElement)
newtype IamfMixPresentationHandle = IamfMixPresentationHandle (ForeignPtr AVIAMFMixPresentation)
newtype IamfParamDefinitionHandle = IamfParamDefinitionHandle (ForeignPtr AVIAMFParamDefinition)

#{enum CInt,
 , avIamfAudioElementTypeChannel      = AV_IAMF_AUDIO_ELEMENT_TYPE_CHANNEL
 , avIamfAudioElementTypeScene        = AV_IAMF_AUDIO_ELEMENT_TYPE_SCENE
 , avIamfAmbisonicsModeMono           = AV_IAMF_AMBISONICS_MODE_MONO
 , avIamfAmbisonicsModeProjection     = AV_IAMF_AMBISONICS_MODE_PROJECTION
 , avIamfParamDefMixGain              = AV_IAMF_PARAMETER_DEFINITION_MIX_GAIN
 , avIamfParamDefDemixing             = AV_IAMF_PARAMETER_DEFINITION_DEMIXING
 , avIamfParamDefReconGain            = AV_IAMF_PARAMETER_DEFINITION_RECON_GAIN
 , avIamfAnimationTypeStep            = AV_IAMF_ANIMATION_TYPE_STEP
 , avIamfAnimationTypeLinear          = AV_IAMF_ANIMATION_TYPE_LINEAR
 , avIamfAnimationTypeBezier          = AV_IAMF_ANIMATION_TYPE_BEZIER
 , avIamfHeadphonesModeStereo         = AV_IAMF_HEADPHONES_MODE_STEREO
 , avIamfHeadphonesModeBinaural       = AV_IAMF_HEADPHONES_MODE_BINAURAL
 , avIamfSubmixLayoutTypeLoudspeakers = AV_IAMF_SUBMIX_LAYOUT_TYPE_LOUDSPEAKERS
 , avIamfSubmixLayoutTypeBinaural     = AV_IAMF_SUBMIX_LAYOUT_TYPE_BINAURAL
 }

avIamfLayerFlagReconGain :: CUInt
avIamfLayerFlagReconGain = #{const AV_IAMF_LAYER_FLAG_RECON_GAIN}

aeNbLayersOffset :: Int
aeNbLayersOffset = #{offset AVIAMFAudioElement, nb_layers}

aeTypeOffset :: Int
aeTypeOffset = #{offset AVIAMFAudioElement, audio_element_type}

aeDefaultWOffset :: Int
aeDefaultWOffset = #{offset AVIAMFAudioElement, default_w}

layerChLayoutOffset :: Int
layerChLayoutOffset = #{offset AVIAMFLayer, ch_layout}

layerFlagsOffset :: Int
layerFlagsOffset = #{offset AVIAMFLayer, flags}

layerAmbisonicsModeOffset :: Int
layerAmbisonicsModeOffset = #{offset AVIAMFLayer, ambisonics_mode}

mpNbSubmixesOffset :: Int
mpNbSubmixesOffset = #{offset AVIAMFMixPresentation, nb_submixes}

mpAnnotationsOffset :: Int
mpAnnotationsOffset = #{offset AVIAMFMixPresentation, annotations}

submixNbElementsOffset :: Int
submixNbElementsOffset = #{offset AVIAMFSubmix, nb_elements}

submixNbLayoutsOffset :: Int
submixNbLayoutsOffset = #{offset AVIAMFSubmix, nb_layouts}

submixOutputMixConfigOffset :: Int
submixOutputMixConfigOffset = #{offset AVIAMFSubmix, output_mix_config}

submixDefaultMixGainOffset :: Int
submixDefaultMixGainOffset = #{offset AVIAMFSubmix, default_mix_gain}

seAudioElementIdOffset :: Int
seAudioElementIdOffset = #{offset AVIAMFSubmixElement, audio_element_id}

seElementMixConfigOffset :: Int
seElementMixConfigOffset = #{offset AVIAMFSubmixElement, element_mix_config}

seDefaultMixGainOffset :: Int
seDefaultMixGainOffset = #{offset AVIAMFSubmixElement, default_mix_gain}

seHeadphonesModeOffset :: Int
seHeadphonesModeOffset = #{offset AVIAMFSubmixElement, headphones_rendering_mode}

seAnnotationsOffset :: Int
seAnnotationsOffset = #{offset AVIAMFSubmixElement, annotations}

slLayoutTypeOffset :: Int
slLayoutTypeOffset = #{offset AVIAMFSubmixLayout, layout_type}

slSoundSystemOffset :: Int
slSoundSystemOffset = #{offset AVIAMFSubmixLayout, sound_system}

slIntegratedLoudnessOffset :: Int
slIntegratedLoudnessOffset = #{offset AVIAMFSubmixLayout, integrated_loudness}

slDigitalPeakOffset :: Int
slDigitalPeakOffset = #{offset AVIAMFSubmixLayout, digital_peak}

slTruePeakOffset :: Int
slTruePeakOffset = #{offset AVIAMFSubmixLayout, true_peak}

pdSubblocksOffsetOffset :: Int
pdSubblocksOffsetOffset = #{offset AVIAMFParamDefinition, subblocks_offset}

pdSubblockSizeOffset :: Int
pdSubblockSizeOffset = #{offset AVIAMFParamDefinition, subblock_size}

pdParameterIdOffset :: Int
pdParameterIdOffset = #{offset AVIAMFParamDefinition, parameter_id}

pdParameterRateOffset :: Int
pdParameterRateOffset = #{offset AVIAMFParamDefinition, parameter_rate}

pdDurationOffset :: Int
pdDurationOffset = #{offset AVIAMFParamDefinition, duration}

pdConstSubblockDurationOffset :: Int
pdConstSubblockDurationOffset = #{offset AVIAMFParamDefinition, constant_subblock_duration}

mgSubblockDurationOffset :: Int
mgSubblockDurationOffset = #{offset AVIAMFMixGain, subblock_duration}

mgAnimationTypeOffset :: Int
mgAnimationTypeOffset = #{offset AVIAMFMixGain, animation_type}

mgStartPointValueOffset :: Int
mgStartPointValueOffset = #{offset AVIAMFMixGain, start_point_value}

allocateIamfAudioElement :: IO IamfAudioElementHandle
allocateIamfAudioElement = do
    ptr <- c_av_iamf_audio_element_alloc
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "Failed to allocate AVIAMFAudioElement"
        else do
            let finalizer = alloca $ \pPtr -> do
                    pokeByteOff (castPtr pPtr) (0 :: Int) ptr
                    c_av_iamf_audio_element_free pPtr
            fptr <- newForeignPtr ptr finalizer
            return $ IamfAudioElementHandle fptr

freeIamfAudioElement :: IamfAudioElementHandle -> IO ()
freeIamfAudioElement (IamfAudioElementHandle fptr) = finalizeForeignPtr fptr

withIamfAudioElement :: (IamfAudioElementHandle -> IO a) -> IO a
withIamfAudioElement = bracket allocateIamfAudioElement freeIamfAudioElement

getIamfAudioElementPtr :: IamfAudioElementHandle -> Ptr AVIAMFAudioElement
getIamfAudioElementPtr (IamfAudioElementHandle fptr) = unsafeForeignPtrToPtr fptr

allocateIamfMixPresentation :: IO IamfMixPresentationHandle
allocateIamfMixPresentation = do
    ptr <- c_av_iamf_mix_presentation_alloc
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "Failed to allocate AVIAMFMixPresentation"
        else do
            let finalizer = alloca $ \pPtr -> do
                    pokeByteOff (castPtr pPtr) (0 :: Int) ptr
                    c_av_iamf_mix_presentation_free pPtr
            fptr <- newForeignPtr ptr finalizer
            return $ IamfMixPresentationHandle fptr

freeIamfMixPresentation :: IamfMixPresentationHandle -> IO ()
freeIamfMixPresentation (IamfMixPresentationHandle fptr) = finalizeForeignPtr fptr

withIamfMixPresentation :: (IamfMixPresentationHandle -> IO a) -> IO a
withIamfMixPresentation = bracket allocateIamfMixPresentation freeIamfMixPresentation

getIamfMixPresentationPtr :: IamfMixPresentationHandle -> Ptr AVIAMFMixPresentation
getIamfMixPresentationPtr (IamfMixPresentationHandle fptr) = unsafeForeignPtrToPtr fptr

allocateIamfParamDefinition :: CInt -> CUInt -> IO IamfParamDefinitionHandle
allocateIamfParamDefinition paramType nbSubblocks = do
    ptr <- c_av_iamf_param_definition_alloc paramType nbSubblocks nullPtr
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "Failed to allocate AVIAMFParamDefinition"
        else do
            fptr <- newForeignPtr ptr (c_av_free (castPtr ptr))
            return $ IamfParamDefinitionHandle fptr

freeIamfParamDefinition :: IamfParamDefinitionHandle -> IO ()
freeIamfParamDefinition (IamfParamDefinitionHandle fptr) = finalizeForeignPtr fptr

withIamfParamDefinition :: CInt -> CUInt -> (IamfParamDefinitionHandle -> IO a) -> IO a
withIamfParamDefinition paramType nbSubblocks =
    bracket (allocateIamfParamDefinition paramType nbSubblocks) freeIamfParamDefinition

getIamfParamDefinitionPtr :: IamfParamDefinitionHandle -> Ptr AVIAMFParamDefinition
getIamfParamDefinitionPtr (IamfParamDefinitionHandle fptr) = unsafeForeignPtrToPtr fptr

iamfAudioElementAddLayer :: Ptr AVIAMFAudioElement -> IO (Ptr AVIAMFLayer)
iamfAudioElementAddLayer aePtr = do
    ptr <- c_av_iamf_audio_element_add_layer aePtr
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "av_iamf_audio_element_add_layer returned null"
        else return ptr

iamfAudioElementSetType :: Ptr AVIAMFAudioElement -> CInt -> IO ()
iamfAudioElementSetType aePtr t =
    pokeByteOff (castPtr aePtr) aeTypeOffset t

iamfAudioElementSetDefaultW :: Ptr AVIAMFAudioElement -> CUInt -> IO ()
iamfAudioElementSetDefaultW aePtr w =
    pokeByteOff (castPtr aePtr) aeDefaultWOffset w

iamfAudioElementNbLayers :: Ptr AVIAMFAudioElement -> IO Int
iamfAudioElementNbLayers aePtr = do
    n <- peekByteOff (castPtr aePtr) aeNbLayersOffset :: IO CUInt
    return (fromIntegral n)

iamfLayerSetChannelLayout :: Ptr AVIAMFLayer -> ChannelLayout -> IO ()
iamfLayerSetChannelLayout layerPtr layout =
    initChannelLayout (castPtr (layerPtr `plusPtr` layerChLayoutOffset)) layout

iamfLayerSetAmbisonicsMode :: Ptr AVIAMFLayer -> CInt -> IO ()
iamfLayerSetAmbisonicsMode layerPtr mode =
    pokeByteOff (castPtr layerPtr) layerAmbisonicsModeOffset mode

iamfLayerSetFlags :: Ptr AVIAMFLayer -> CUInt -> IO ()
iamfLayerSetFlags layerPtr flags =
    pokeByteOff (castPtr layerPtr) layerFlagsOffset flags

iamfMixPresentationAddSubmix :: Ptr AVIAMFMixPresentation -> IO (Ptr AVIAMFSubmix)
iamfMixPresentationAddSubmix mpPtr = do
    ptr <- c_av_iamf_mix_presentation_add_submix mpPtr
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "av_iamf_mix_presentation_add_submix returned null"
        else return ptr

iamfMixPresentationSetAnnotation :: Ptr AVIAMFMixPresentation -> String -> String -> IO ()
iamfMixPresentationSetAnnotation mpPtr lang label = do
    let dictPtrPtr = castPtr (mpPtr `plusPtr` mpAnnotationsOffset) :: Ptr (Ptr AVDictionary)
    withCString lang $ \cLang ->
        withCString label $ \cLabel -> do
            ret <- c_av_dict_set dictPtrPtr cLang cLabel 0
            _ <- errorNonNegative "av_dict_set(mix_presentation)" (fromIntegral ret)
            return ()

iamfMixPresentationNbSubmixes :: Ptr AVIAMFMixPresentation -> IO Int
iamfMixPresentationNbSubmixes mpPtr = do
    n <- peekByteOff (castPtr mpPtr) mpNbSubmixesOffset :: IO CUInt
    return (fromIntegral n)

iamfSubmixAddElement :: Ptr AVIAMFSubmix -> IO (Ptr AVIAMFSubmixElement)
iamfSubmixAddElement submixPtr = do
    ptr <- c_av_iamf_submix_add_element submixPtr
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "av_iamf_submix_add_element returned null"
        else return ptr

iamfSubmixAddLayout :: Ptr AVIAMFSubmix -> IO (Ptr AVIAMFSubmixLayout)
iamfSubmixAddLayout submixPtr = do
    ptr <- c_av_iamf_submix_add_layout submixPtr
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "av_iamf_submix_add_layout returned null"
        else return ptr

iamfSubmixSetDefaultMixGain :: Ptr AVIAMFSubmix -> AVRational -> IO ()
iamfSubmixSetDefaultMixGain submixPtr gain =
    pokeByteOff (castPtr submixPtr) submixDefaultMixGainOffset gain

iamfSubmixSetOutputMixConfig :: Ptr AVIAMFSubmix -> Ptr AVIAMFParamDefinition -> IO ()
iamfSubmixSetOutputMixConfig submixPtr paramPtr = do
    oldPtr <- peekByteOff (castPtr submixPtr) submixOutputMixConfigOffset :: IO (Ptr AVIAMFParamDefinition)
    when (oldPtr /= nullPtr && oldPtr /= paramPtr) $
        c_av_free (castPtr oldPtr)
    pokeByteOff (castPtr submixPtr) submixOutputMixConfigOffset paramPtr

iamfSubmixNbElements :: Ptr AVIAMFSubmix -> IO Int
iamfSubmixNbElements submixPtr = do
    n <- peekByteOff (castPtr submixPtr) submixNbElementsOffset :: IO CUInt
    return (fromIntegral n)

iamfSubmixNbLayouts :: Ptr AVIAMFSubmix -> IO Int
iamfSubmixNbLayouts submixPtr = do
    n <- peekByteOff (castPtr submixPtr) submixNbLayoutsOffset :: IO CUInt
    return (fromIntegral n)

iamfSubmixElementSetAudioElementId :: Ptr AVIAMFSubmixElement -> CUInt -> IO ()
iamfSubmixElementSetAudioElementId sePtr aeId =
    pokeByteOff (castPtr sePtr) seAudioElementIdOffset aeId

iamfSubmixElementSetDefaultMixGain :: Ptr AVIAMFSubmixElement -> AVRational -> IO ()
iamfSubmixElementSetDefaultMixGain sePtr gain =
    pokeByteOff (castPtr sePtr) seDefaultMixGainOffset gain

iamfSubmixElementSetHeadphonesMode :: Ptr AVIAMFSubmixElement -> CInt -> IO ()
iamfSubmixElementSetHeadphonesMode sePtr mode =
    pokeByteOff (castPtr sePtr) seHeadphonesModeOffset mode

iamfSubmixElementSetAnnotation :: Ptr AVIAMFSubmixElement -> String -> String -> IO ()
iamfSubmixElementSetAnnotation sePtr lang label = do
    let dictPtrPtr = castPtr (sePtr `plusPtr` seAnnotationsOffset) :: Ptr (Ptr AVDictionary)
    withCString lang $ \cLang ->
        withCString label $ \cLabel -> do
            ret <- c_av_dict_set dictPtrPtr cLang cLabel 0
            _ <- errorNonNegative "av_dict_set(submix_element)" (fromIntegral ret)
            return ()

iamfSubmixElementSetMixConfig :: Ptr AVIAMFSubmixElement -> Ptr AVIAMFParamDefinition -> IO ()
iamfSubmixElementSetMixConfig sePtr paramPtr = do
    oldPtr <- peekByteOff (castPtr sePtr) seElementMixConfigOffset :: IO (Ptr AVIAMFParamDefinition)
    when (oldPtr /= nullPtr && oldPtr /= paramPtr) $
        c_av_free (castPtr oldPtr)
    pokeByteOff (castPtr sePtr) seElementMixConfigOffset paramPtr

iamfSubmixLayoutSetType :: Ptr AVIAMFSubmixLayout -> CInt -> IO ()
iamfSubmixLayoutSetType slPtr lt =
    pokeByteOff (castPtr slPtr) slLayoutTypeOffset lt

iamfSubmixLayoutSetSoundSystem :: Ptr AVIAMFSubmixLayout -> ChannelLayout -> IO ()
iamfSubmixLayoutSetSoundSystem slPtr layout =
    initChannelLayout (castPtr (slPtr `plusPtr` slSoundSystemOffset)) layout

iamfSubmixLayoutSetLoudness :: Ptr AVIAMFSubmixLayout -> AVRational -> AVRational -> AVRational -> IO ()
iamfSubmixLayoutSetLoudness slPtr intLoud digPeak truePeak = do
    pokeByteOff (castPtr slPtr) slIntegratedLoudnessOffset intLoud
    pokeByteOff (castPtr slPtr) slDigitalPeakOffset digPeak
    pokeByteOff (castPtr slPtr) slTruePeakOffset truePeak

iamfParamDefinitionGetSubblock :: Ptr AVIAMFParamDefinition -> Int -> IO (Ptr ())
iamfParamDefinitionGetSubblock parPtr idx = do
    off <- peekByteOff (castPtr parPtr) pdSubblocksOffsetOffset :: IO CSize
    sz <- peekByteOff (castPtr parPtr) pdSubblockSizeOffset :: IO CSize
    return (castPtr parPtr `plusPtr` (fromIntegral off + idx * fromIntegral sz))

allocRawMixGainParam :: CUInt -> CUInt -> CUInt -> AVRational -> IO (Ptr AVIAMFParamDefinition)
allocRawMixGainParam paramId paramRate dur gain = do
    ptr <- c_av_iamf_param_definition_alloc avIamfParamDefMixGain 1 nullPtr
    if ptr == nullPtr
        then throwIO $ FFmpegError (-1) "av_iamf_param_definition_alloc returned null"
        else do
            pokeByteOff (castPtr ptr) pdParameterIdOffset paramId
            pokeByteOff (castPtr ptr) pdParameterRateOffset paramRate
            pokeByteOff (castPtr ptr) pdDurationOffset dur
            pokeByteOff (castPtr ptr) pdConstSubblockDurationOffset dur
            sbPtr <- iamfParamDefinitionGetSubblock ptr 0
            pokeByteOff (castPtr sbPtr) mgSubblockDurationOffset dur
            pokeByteOff (castPtr sbPtr) mgAnimationTypeOffset avIamfAnimationTypeStep
            pokeByteOff (castPtr sbPtr) mgStartPointValueOffset gain
            return ptr
