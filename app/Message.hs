{-# LANGUAGE DeriveGeneric #-}

module Message (
  Message (..),
  encodeMsg,
  decodeMsg,
  BeaconMessage (..),
  encodeBeacon,
  decodeBeacon,
) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)

data Message = Message {to :: Integer, msg :: String}
  deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

decodeMsg :: BL.ByteString -> Maybe Message
decodeMsg = decode

encodeMsg :: Message -> BL.ByteString
encodeMsg = encode

data BeaconMessage = BeaconMessage
  { beaconType :: String
  , beaconVersion :: String
  , beaconPort :: Integer
  , beaconTimestamp :: Integer
  }
  deriving (Show, Generic)

instance FromJSON BeaconMessage
instance ToJSON BeaconMessage

decodeBeacon :: BL.ByteString -> Maybe BeaconMessage
decodeBeacon = decode

encodeBeacon :: BeaconMessage -> BL.ByteString
encodeBeacon = encode
