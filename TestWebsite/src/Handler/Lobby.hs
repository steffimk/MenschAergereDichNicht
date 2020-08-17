{-# LANGUAGE OverloadedStrings #-}
module Handler.Lobby where

import Import
import Data.Aeson

newtype Lobby = Lobby Text

instance ToJSON Lobby where
    toJSON (Lobby t) = object ["lobbyname" .= t]
instance FromJSON Lobby where
    parseJSON = withObject "Lobby" $ \o -> Lobby <$> o .: "lobbyname"

postLobbyCreationR :: Handler Value
postLobbyCreationR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    lobby <- requireCheckJsonBody :: Handler Lobby

    returnJson lobby
