{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Config
import Control.Concurrent
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import Data.Maybe
import Network.WebSockets
import Network.Http.Client

main :: IO ()
main = do
  mvar <- newMVar ""
  _    <- forkIO $ runClient (komzentAddress config) (komzentPort config) (komzentPath config) (kommandozentraleClient mvar)
  runServer (scottyAddress config) (scottyPort config) (scottyApp mvar)

kommandozentraleClient :: MVar ByteString -> ClientApp ()
kommandozentraleClient mvar con = do
  music <- decode <$> receiveData con
  case music of
    Just m@(Song _) -> void $ swapMVar mvar (encode m)
    Nothing         -> return ()
  kommandozentraleClient mvar con

scottyApp :: MVar ByteString -> ServerApp
scottyApp mvar req = do
  con <- acceptRequest req
  roomStatus <- fromMaybe Unknown <$> get (spaceApiURL config) jsonHandler
  sendTextData con (encode roomStatus)
  song <- tryReadMVar mvar
  case song of 
    Just x  -> sendTextData con x
    Nothing -> return ()
