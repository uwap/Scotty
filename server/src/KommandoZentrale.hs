{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module KommandoZentrale where

import Types
import Config
import Data.Aeson
import Control.Monad
import Control.Concurrent
import Control.Lens hiding ((.=))
import Network.WebSockets

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

{-| This thread updates the State whenever the
    KommandoZentrale pushes new JSON Data.
 -}
kommandozentraleThread :: MVar ScottyState -> ClientApp ()
kommandozentraleThread mvar con = do
  music <- decode <$> receiveData con
  case music of
    Just m@(Song _) -> modifyMVarMasked_ mvar (\s -> return $ s & mpd .~ m)
    Nothing         -> return ()
  kommandozentraleThread mvar con

{-| Launches the Thread which listens to the KommandoZentrale Websocket and
    updates the State whenever new data is pushed.
 -}
launchKommandoZentraleThread :: MVar ScottyState -> IO ()
launchKommandoZentraleThread mvar = void . forkIO $
  runClient (komzentAddress config) (komzentPort config) (komzentPath config) (kommandozentraleThread mvar)
