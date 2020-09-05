{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Lobby where

import           Control.Monad.State
import           Data.Aeson
import           GHC.Generics
import           Import
import           Prelude             hiding ((++))

type CSRF_Token = Text
data Lobby = Lobby { lobbyname :: Text, player_token :: CSRF_Token } deriving Show

instance ToJSON Lobby where
    toJSON (Lobby n p_t) = object ["lobbyname" .= n, "player_token" .= p_t]
instance FromJSON Lobby where
    parseJSON = withObject "Lobby" $ \o -> Lobby <$> o .: "lobbyname" <*> o.: "player_token"

data OpenLobbies = OpenLobbies { open_lobbies :: [Lobby] } deriving (Generic, Show)

instance FromJSON OpenLobbies
instance ToJSON OpenLobbies

-- instance ToJSON OpenLobbies where
--     toJSON (OpenLobbies n) = object ["open_lobbies" .= n]
-- instance FromJSON OpenLobbies where
--     parseJSON = withObject "OpenLobbies" $ \o -> OpenLobbies <$> o .: "open_lobbies"


addLobby :: Lobby -> State OpenLobbies Int
addLobby lobby = do
    currentState <- Control.Monad.State.get
    let openLobbies = open_lobbies currentState
    put (currentState { open_lobbies = openLobbies ++ [lobby] })
    return ((Prelude.length openLobbies) + 1)

getOpenLobbies :: State OpenLobbies OpenLobbies
getOpenLobbies = do
    currentState <- Control.Monad.State.get
    return currentState

type MyMonad1 = StateT OpenLobbies (HandlerFor App)

postLobbyCreationR :: Handler Value
postLobbyCreationR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    lobby <- requireCheckJsonBody :: Handler Lobby
    let countLobbies = addLobby lobby
    case countLobbies of
        1         -> returnJson lobby
        otherwise -> returnJson countLobbies
    -- let openLobbies = openLobbies { open_lobbies = (open_lobbies openLobbies) ++ [lobby] }
    -- openLobbies <- lobby
    --nuffin <- addLobby lobby
    -- addLobby lobby

    --returnJson lobby

-- postLobbyCreationR :: HandlerFor App Value
-- postLobbyCreationR = do
--     -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
--     lobby <- requireCheckJsonBody :: Handler Lobby
--     -- let openLobbies = openLobbies { open_lobbies = (open_lobbies openLobbies) ++ [lobby] }
--     -- addLobby lobby

--     returnJson lobby

postCommentR :: Handler Value
postCommentR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    comment <- requireCheckJsonBody :: Handler OpenLobbies

    returnJson comment

getCommentR :: Handler Value
getCommentR = do
    openLobbies <- Current.Monad.State.get
    returnJson openLobbies
