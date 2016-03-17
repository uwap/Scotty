{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T

data ScottyConfig = ScottyConfig
                  { scottyAddress  :: String
                  , scottyPort     :: Int
                  , spaceApiURL    :: B.ByteString
                  , komzentAddress :: String
                  , komzentPort    :: Int
                  , komzentPath    :: FilePath
                  } deriving (Show)

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



data MPDStatus = Song T.Text
              deriving (Show)

instance FromJSON MPDStatus where
  parseJSON (Object v) = do
    state    <- v .: "state"
    metadata <- v .: "metadata"
    mtype    <- metadata .: "type"
    switch   <- v .: "switch"
    case (mtype, switch) of
      (String "music", String "mpd") -> Song <$> state .: "song"
      _                              -> fail "Not an MPD Status"
  parseJSON _ = fail "Not an MPD Status"

instance ToJSON MPDStatus where
  toJSON (Song x) = object ["mpd" .= object [ "song" .= x ]]



data ScottyState = ScottyState
                  { _room :: RoomStatus
                  , _mpd  :: MPDStatus
                  } deriving (Show)
makeLenses ''ScottyState
