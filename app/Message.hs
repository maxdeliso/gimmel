{-# LANGUAGE DeriveGeneric #-}

module Message
  ( Message
  , encodeMsg
  , decodeMsg
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import GHC.Generics (Generic)

data Message = Message { to :: Integer, msg :: String }
  deriving (Show, Generic)

instance FromJSON Message
instance ToJSON Message

decodeMsg :: BL.ByteString -> Maybe Message
decodeMsg = decode

encodeMsg :: Message -> BL.ByteString
encodeMsg = encode
