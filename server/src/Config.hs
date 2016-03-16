{-# LANGUAGE OverloadedStrings #-}
module Config where

import Types

config :: ScottyConfig
config = ScottyConfig
       { scottyAddress = "127.0.0.1" -- The Address to bind
       , scottyPort    = 5607        -- The Port to listen on
       , spaceApiURL   = "http://state.maschinendeck.org/spaceapi.php"
       }
