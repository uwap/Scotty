{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module RoomStatus where

import Types
import Config
import Data.Maybe
import Control.Concurrent
import Control.Lens hiding ((.=))
import Control.Monad
import Network.Http.Client

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
