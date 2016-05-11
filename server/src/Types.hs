{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.WebSockets

{-| The Config Type initialized in Config.hs -}
data ScottyConfig = ScottyConfig
                  { scottyAddress  :: String       -- ^ The Adress the scotty websocket shall bind to
                  , scottyPort     :: Int          -- ^ The Port the scotty websocket shall bind to
                  , spaceApiURL    :: B.ByteString -- ^ The URL to the SpaceAPI  
                  , komzentAddress :: String       -- ^ The Adress of the KommandoZentrale
                  , komzentPort    :: Int          -- ^ The Port of the KommandoZentrale
                  , komzentPath    :: FilePath     -- ^ The Path of the KommandoZentrale
                  } deriving (Show)

{-| The RoomStatus is either Open or Closed.
    But as in constructive logic there might be some third option.
 -}
data RoomStatus = RoomOpen | RoomClosed | Unknown
              deriving (Show)

{-| When Talking to the Space API data come in as
    JSON files with a state.open property set to a
    nullable boolean. true is the room is open.
    false if it is closed. null if unknown.
    This is a smart constructor for it.
 -}
mkRoomStatus :: Value -> RoomStatus
mkRoomStatus (Bool True)  = RoomOpen
mkRoomStatus (Bool False) = RoomClosed
mkRoomStatus _ = Unknown

{-| Decode the Space API JSON
 
    Note: Because we only need to deserialize from the SpaceAPI and serialize for
    the scotty client, we don't hold the law `x = decode (encode x)` resp. `x = encode (decode x)`
 -}
instance FromJSON RoomStatus where
  parseJSON (Object v) = mkRoomStatus <$> (v .: "state" >>= (.: "open"))
  parseJSON v          = return $ mkRoomStatus v

{-| Convert RoomStatus to JSON for the scotty client
 
    Note: Because we only need to deserialize from the SpaceAPI and serialize for
    the scotty client, we don't hold the law `x = decode (encode x)` resp. `x = encode (decode x)`
 -}
instance ToJSON RoomStatus where
  toJSON RoomOpen   = object ["room" .= object [ "state" .= T.pack "open" ]]
  toJSON RoomClosed = object ["room" .= object [ "state" .= T.pack "closed" ]]
  toJSON Unknown    = object ["room" .= object [ "state" .= T.pack "unknown"]]



{-| The Status Infos of the MPD.
    For now we just hold the song name
 -}
data MPDStatus = Song T.Text
              deriving (Show)

{-| Decodes the JSON from the KommandoZentrale and extracts the song name infos.
 
    Note: Because we only need to deserialize from the KommandoZentrale and serialize for
    the scotty client, we don't hold the law `x = decode (encode x)` resp. `x = encode (decode x)`
 -}
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

{-| Encodes the JSON for the Scotty Client.
 
    Note: Because we only need to deserialize from the KommandoZentrale and serialize for
    the scotty client, we don't hold the law `x = decode (encode x)` resp. `x = encode (decode x)`
 -}
instance ToJSON MPDStatus where
  toJSON (Song x) = object ["mpd" .= object [ "song" .= x ]]



{-| All Data to be stored about one Client -}
data Client = Client Connection

data ScottyState = ScottyState
                  { _room    :: RoomStatus
                  , _mpd     :: MPDStatus
                  , _clients :: [Client]
                  }
makeLenses ''ScottyState
