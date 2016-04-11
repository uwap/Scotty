{-# LANGUAGE ScopedTypeVariables #-}
module Client where

import Types
import KommandoZentrale ()
import RoomStatus ()
import Control.Lens
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.Maybe
import Data.Aeson
import Network.WebSockets

{-| Sends the client the information stored in the state -}
updateClient :: Client -> ScottyState -> IO ()
updateClient (Client con) s = do
  sendTextData con (s ^. room & encode)
  sendTextData con (s ^. mpd  & encode)

{-| Runs updateClient for all clients in the state
    and also updates the state by removing all disconnected
    or errorous clients
-}
updateAllClients :: MVar ScottyState -> IO ()
updateAllClients mvar = do
  modifyMVarMasked_ mvar $ \s -> do
    ns <- fmap catMaybes . forM (s ^. clients) $ \c ->
      either (\(_ :: SomeException) -> Nothing) Just <$> try (updateClient c s >> return c)
    return (s & clients .~ ns)
