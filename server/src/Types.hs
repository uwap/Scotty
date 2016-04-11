{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Lens hiding ((.=))
import qualified Data.ByteString as B
import qualified Data.Text as T
import Network.WebSockets

{-| The Config Type initialized in Config.hs -}
data ScottyConfig = ScottyConfig
                  { scottyAddress  :: String       -- ^ The Adress the scotty websocket shall bind to
                  , scottyPort     :: Int          -- ^ The Port the scotty websocket shall bind to
                  , spaceApiURL    :: B.ByteString -- ^ The URL to the SpaceAPI  
                  , komzentAddress :: String       -- ^ The Adress of the KommandoZentrale
                  , komzentPort    :: Int          -- ^ The Port of the KommandoZentrale
                  , komzentPath    :: FilePath     -- ^ The Path of the KommandoZentrale
                  } deriving (Show)

{-| The RoomStatus is either Open or Closed.
    But as in constructive logic there might be some third option.
 -}
data RoomStatus = RoomOpen | RoomClosed | Unknown
              deriving (Show)

{-| The Status Infos of the MPD.
    For now we just hold the song name
 -}
data MPDStatus = Song T.Text
              deriving (Show)

{-| All Data to be stored about one Client -}
data Client = Client Connection

data ScottyState = ScottyState
                  { _room    :: RoomStatus
                  , _mpd     :: MPDStatus
                  , _clients :: [Client]
                  }
makeLenses ''ScottyState
