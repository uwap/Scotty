{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module RoomStatus where

import Types
import Config
import Data.Aeson
import Data.Maybe
import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad
import Network.Http.Client
import qualified Data.Text as T

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


spaceAPIupdateThread :: MVar ScottyState -> IO ()
spaceAPIupdateThread mvar = do
  roomStatus <- fromMaybe Unknown <$> get (spaceApiURL config) jsonHandler
  modifyMVarMasked_ mvar (\s -> return $ s & room .~ roomStatus)
  threadDelay 60000000 -- Wait a minute

{-| Launch a thread that requests the RoomStatus from the SpaceAPI every 60 seconds
    and updates the State whenever it has changed.
 -}
launchSpaceAPIThread :: MVar ScottyState -> IO ()
launchSpaceAPIThread = void . forkIO . spaceAPIupdateThread
