{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Config
import Client
import RoomStatus
import KommandoZentrale
import Control.Concurrent
import Control.Lens
import Network.WebSockets

main :: IO ()
main = do
  mvar <- newMVar $ ScottyState Unknown (Song "Keine Musik") []
  launchSpaceAPIThread mvar
  launchKommandoZentraleThread mvar
  runServer (scottyAddress config) (scottyPort config) (scottyApp mvar)

scottyApp :: MVar ScottyState -> ServerApp
scottyApp mvar req = do
    con <- acceptRequest req
    forkPingThread con 30
    state <- tryReadMVar mvar
    case state of
      Just s -> do
        let client = Client con
        updateClient client s
        modifyMVarMasked_ mvar (\st -> return $ st & clients <>~ pure client)
      Nothing -> putStrLn "Fatal Error: Cannot read state"
