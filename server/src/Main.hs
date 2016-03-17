{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Config
import Control.Concurrent
import Control.Lens
import Data.Aeson
import Data.Maybe
import Network.WebSockets
import Network.Http.Client

main :: IO ()
main = do
  mvar <- newMVar $ ScottyState Unknown (Song "Keine Musik")
  _    <- forkIO  $ updateSpaceAPI mvar
  _    <- forkIO  $ runClient (komzentAddress config) (komzentPort config) (komzentPath config) (kommandozentraleClient mvar)
  runServer (scottyAddress config) (scottyPort config) (scottyApp mvar)

updateSpaceAPI :: MVar ScottyState -> IO ()
updateSpaceAPI mvar = do
  roomStatus <- fromMaybe Unknown <$> get (spaceApiURL config) jsonHandler
  modifyMVarMasked_ mvar (\s -> return $ s & room .~ roomStatus)
  threadDelay 60000000 -- Wait a minute

kommandozentraleClient :: MVar ScottyState -> ClientApp ()
kommandozentraleClient mvar con = do
  music <- decode <$> receiveData con
  case music of
    Just m@(Song _) -> modifyMVarMasked_ mvar (\s -> return $ s & mpd .~ m)
    Nothing         -> return ()
  kommandozentraleClient mvar con

scottyApp :: MVar ScottyState -> ServerApp
scottyApp mvar req = do
    con <- acceptRequest req
    forkPingThread con 30
    loop con
  where
    loop con = do
      roomStatus <- fromMaybe Unknown <$> get (spaceApiURL config) jsonHandler
      sendTextData con (encode roomStatus)
      state <- tryReadMVar mvar
      case state of 
        Just s  -> do sendTextData con (s ^. room & encode)
                      sendTextData con (s ^. mpd  & encode)
        Nothing -> putStrLn "Fatal Error. Cannot read state"
      loop con
