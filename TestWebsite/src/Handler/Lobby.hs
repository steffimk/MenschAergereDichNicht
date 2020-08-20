{-# LANGUAGE OverloadedStrings #-}
module Handler.Lobby where

import Import
import Data.Aeson
import Prelude hiding ((++))

type CSRF_Token = Text
data Lobby = Lobby { lobbyname :: Text, player_token :: CSRF_Token } deriving Show

instance ToJSON Lobby where
    toJSON (Lobby n p_t) = object ["lobbyname" .= n, "player_token" .= p_t]
instance FromJSON Lobby where
    parseJSON = withObject "Lobby" $ \o -> Lobby <$> o .: "lobbyname" <*> o.: "player_token"

data OpenLobbies = OpenLobbies { open_lobbies :: [Lobby] } deriving Show
openLobbies :: OpenLobbies
openLobbies = OpenLobbies []

addLobby :: Lobby -> OpenLobbies
-- addLobby lobby = OpenLobbies ((open_lobbies openLobbies) ++ [lobby])
addLobby lobby = openLobbies { open_lobbies = (open_lobbies openLobbies) ++ [lobby] }

postLobbyCreationR :: Handler Value
postLobbyCreationR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    lobby <- requireCheckJsonBody :: Handler Lobby
    -- addLobby lobby

    returnJson lobby
