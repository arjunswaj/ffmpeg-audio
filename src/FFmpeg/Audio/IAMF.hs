module FFmpeg.Audio.IAMF
    ( IamfContainer(..)
    , IamfSubstreamCodec(..)
    , IamfElementKind(..)
    , IamfHeadphonesMode(..)
    , IamfElement(..)
    , IamfConfig(..)
    , defaultIamfConfig
    , encodeIamf
    , encodeIamfBS
    ) where

import Data.ByteString qualified as BS
import FFmpeg.Audio.Internal.IamfMux
    ( IamfConfig(..)
    , IamfContainer(..)
    , IamfElement(..)
    , IamfElementKind(..)
    , IamfHeadphonesMode(..)
    , IamfSubstreamCodec(..)
    , defaultIamfConfig
    , encodeIamfToBS
    , encodeIamfToFile
    )

encodeIamf :: FilePath -> IamfConfig -> [IamfElement] -> IO ()
encodeIamf = encodeIamfToFile

encodeIamfBS :: IamfConfig -> [IamfElement] -> IO BS.ByteString
encodeIamfBS = encodeIamfToBS
