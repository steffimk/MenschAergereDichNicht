{-# LANGUAGE OverloadedStrings #-}
module Handler.Lobby where

import           Control.Monad.State
import           Data.Aeson
import           Import
import           Prelude             hiding ((++))

type CSRF_Token = Text
data Lobby = Lobby { lobbyname :: Text, player_token :: CSRF_Token } deriving Show

instance ToJSON Lobby where
    toJSON (Lobby n p_t) = object ["lobbyname" .= n, "player_token" .= p_t]
instance FromJSON Lobby where
    parseJSON = withObject "Lobby" $ \o -> Lobby <$> o .: "lobbyname" <*> o.: "player_token"

-- data OpenLobbies = OpenLobbies { open_lobbies :: [Lobby] } deriving Show
-- openLobbies :: OpenLobbies
-- openLobbies = OpenLobbies []

data OpenLobbies2 = OpenLobbies2 { open_lobbies :: [Lobby] }

addLobby :: Lobby -> State OpenLobbies2 ()
addLobby lobby = do
    currentState <- Control.Monad.State.get
    let openLobbies = open_lobbies currentState
    put (currentState { open_lobbies = openLobbies ++ [lobby]})
    return ()


-- openLobbies2 :: [Lobby] -> Lobby -> IO OpenLobbies2
-- openLobbies2 lobby = do
--     let new_lobby_list = (open_lobbies openLobbies2) ++ [lobby]
--     return $ OpenLobbies2 { open_lobbies = new_lobby_list }

-- addLobby :: Lobby -> OpenLobbies2
-- -- addLobby lobby = OpenLobbies ((open_lobbies openLobbies) ++ [lobby])
-- addLobby lobby = openLobbies { open_lobbies = (open_lobbies openLobbies) ++ [lobby] }

postLobbyCreationR :: Handler Value
postLobbyCreationR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    lobby <- requireCheckJsonBody :: Handler Lobby
    -- let openLobbies2 = openLobbies { open_lobbies = (open_lobbies openLobbies) ++ [lobby] }
    -- openLobbies <- lobby
    -- nuffin <- addLobby lobby

    returnJson lobby

-- postLobbyCreationR :: Handler Value
-- postLobbyCreationR = do
--     -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
--     lobby <- requireCheckJsonBody :: Handler Lobby
--     -- let openLobbies2 = openLobbies { open_lobbies = (open_lobbies openLobbies) ++ [lobby] }
--     addLobby lobby

--     returnJson lobby
