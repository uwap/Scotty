{-# LANGUAGE OverloadedStrings #-}
module Config where

import Types

config :: ScottyConfig
config = ScottyConfig
       { scottyAddress  = "127.0.0.1" -- The Address to open a websocket to
       , scottyPort     = 5607        -- The Port     "   "  "     "      "
       , spaceApiURL    = "http://state.maschinendeck.org/spaceapi.php"
       -- Kommandozentrale
       , komzentAddress = "127.0.0.1" -- The Address of the Kommandozentrale Websocket
       , komzentPort    = 9000        -- The Port     "  "         "             "
       , komzentPath    = "/"         -- The Path of the Kommandozentrale API
       }
