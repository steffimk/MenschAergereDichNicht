{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Model.LobbyModel where

import           ClassyPrelude.Yesod

type CSRF_Token = Text

data Lobby = Lobby { lobbyname :: Text, player_tokens :: [CSRF_Token] } deriving (Generic, Show)
instance FromJSON Lobby
instance ToJSON Lobby

data LobbyToJoin = LobbyToJoin { lobbynameToJoin :: Text, players_token :: CSRF_Token } deriving (Generic, Show)
instance FromJSON LobbyToJoin
instance ToJSON LobbyToJoin

data LobbyToLeave = LobbyToLeave { lobbynameToLeave :: Text, leaving_players_token :: CSRF_Token } deriving (Generic, Show)
instance FromJSON LobbyToLeave
instance ToJSON LobbyToLeave
