{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T

data ScottyConfig = ScottyConfig
                  { scottyAddress :: String
                  , scottyPort    :: Int
                  , spaceApiURL   :: B.ByteString
                  }

data RoomStatus = RoomOpen | RoomClosed | Unknown
              deriving (Show)

{- When Talking to the Space API data come in as
 - JSON files with a state.open property set to a
 - nullable boolean. true is the room is open.
 - false if it is closed. null if unknown.
 - This is a smart constructor for it.
 -}
mkRoomStatus :: Value -> RoomStatus
mkRoomStatus (Bool True)  = RoomOpen
mkRoomStatus (Bool False) = RoomClosed
mkRoomStatus _ = Unknown

instance FromJSON RoomStatus where
  parseJSON (Object v) = mkRoomStatus <$> (v .: "state" >>= (.: "open"))
  parseJSON v          = return $ mkRoomStatus v

instance ToJSON RoomStatus where
  toJSON RoomOpen   = object ["room" .= object [ "state" .= T.pack "open" ]]
  toJSON RoomClosed = object ["room" .= object [ "state" .= T.pack "closed" ]]
  toJSON Unknown    = object ["room" .= object [ "state" .= T.pack "unknown"]]
