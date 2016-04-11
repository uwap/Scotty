{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Config
import RoomStatus
import KommandoZentrale
import Control.Concurrent
import Control.Lens
import Data.Aeson
import Network.WebSockets

main :: IO ()
main = do
  mvar <- newMVar $ ScottyState Unknown (Song "Keine Musik")
  launchSpaceAPIThread mvar
  launchKommandoZentraleThread mvar
  runServer (scottyAddress config) (scottyPort config) (scottyApp mvar)

scottyApp :: MVar ScottyState -> ServerApp
scottyApp mvar req = do
    con <- acceptRequest req
    forkPingThread con 30
    loop con
  where
    loop con = do
      state <- tryReadMVar mvar
      case state of 
        Just s  -> do sendTextData con (s ^. room & encode)
                      sendTextData con (s ^. mpd  & encode)
        Nothing -> putStrLn "Fatal Error: Cannot read state"
      loop con
