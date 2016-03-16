{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Config
import qualified Data.ByteString.Char8 as C
import Data.Aeson
import Data.Maybe
import Network.WebSockets
import Network.Http.Client

main :: IO ()
main = runServer (scottyAddress config) (scottyPort config) scottyApp

scottyApp :: ServerApp
scottyApp req = do
  con <- acceptRequest req
  roomStatus <- fromMaybe Unknown <$> get (spaceApiURL config) jsonHandler
  sendTextData con (encode roomStatus)
