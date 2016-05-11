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
