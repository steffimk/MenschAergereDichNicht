{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Lobby where

import           Control.Monad.State
import           Data.Aeson
import           Data.List
import           Import
import Model.LobbyModel
import           Prelude             hiding ((++))

addLobby :: Lobby -> [Lobby] -> [Lobby]
addLobby lobby openLobbies = do
    let oldOpenLobbies = openLobbies
    oldOpenLobbies Data.List.++ [lobby]

postLobbyCreationR :: Handler Value
postLobbyCreationR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    lobby <- requireCheckJsonBody :: Handler Lobby
    master <- getYesod
    openLobbies <- liftIO $ readTVarIO $ openLobbiesMaster master
    let player_token = (Prelude.head (player_tokens lobby))
        lobbyToJoin = lobbyname lobby
        openLobbiesPlayerRemoved = addPtToLobbyAndRemoveFromOthers player_token lobbyToJoin openLobbies
        newOpenLobbies = openLobbiesPlayerRemoved Data.List.++ [lobby]
    liftIO $ Prelude.putStrLn ("adding new lobby: " Data.List.++ show lobby)
    liftIO $ atomically $ writeTVar (openLobbiesMaster master) newOpenLobbies

    returnJson lobby

getOpenLobbiesR :: Handler Value
getOpenLobbiesR = do
    master <- getYesod
    openLobbies <- liftIO $ readTVarIO $ openLobbiesMaster master
    liftIO $ Prelude.putStrLn ("open lobbies: " Data.List.++ show openLobbies)
    returnJson openLobbies

rmPtFromLobbies :: CSRF_Token -> [Lobby] -> [Lobby]
rmPtFromLobbies pt lobbies =
    rmEmptyLobbies (Prelude.map (\l -> rmPt pt l) lobbies)
    -- where rmPt player_token lnToLeave lobby = case lnToLeave == (lobbyname lobby) of
    --                                                 True -> case Prelude.elem player_token (player_tokens lobby) of
    --                                                     True -> case (Prelude.length (player_tokens lobby) >= 4) of
    --                                                         True -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(player_tokens lobby) }
    --                                                         _    -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(Data.List.delete player_token (player_tokens lobby)) }
    --                                                     _    -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(Data.List.delete player_token (player_tokens lobby)) }
    --                                                 _    -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(Data.List.delete player_token (player_tokens lobby)) }
    where rmPt player_token lobby = case (Prelude.length (player_tokens lobby) >= 4) of
                                                    True -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(player_tokens lobby) }
                                                    _    -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(Data.List.delete player_token (player_tokens lobby)) }

addPtToLobbyAndRemoveFromOthers :: CSRF_Token -> Text -> [Lobby] -> [Lobby]
addPtToLobbyAndRemoveFromOthers pt ln lobbies =
    rmEmptyLobbies (Prelude.map (\l -> addPt pt ln l) lobbies)
    where addPt player_token lnToJoin lobby = case lnToJoin == (lobbyname lobby) of
                                                    True -> case (Prelude.length (Data.List.delete pt (player_tokens lobby))) <= 3 of
                                                        True -> Lobby { lobbyname=(lobbyname lobby), player_tokens=((Data.List.delete pt (player_tokens lobby)) Data.List.++ [player_token]) }
                                                        _    -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(Data.List.delete pt (player_tokens lobby)) }
                                                    _    -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(Data.List.delete pt (player_tokens lobby)) }

rmEmptyLobbies :: [Lobby] -> [Lobby]
rmEmptyLobbies lobbies =
    Prelude.filter (\l -> player_tokens l /= []) lobbies

postJoinLobbyR :: Handler Value
postJoinLobbyR = do
    lobbyToJoin <- requireCheckJsonBody :: Handler LobbyToJoin
    master <- getYesod
    openLobbies <- liftIO $ readTVarIO $ openLobbiesMaster master
    let newLobbies = addPtToLobbyAndRemoveFromOthers (players_token lobbyToJoin) (lobbynameToJoin lobbyToJoin) openLobbies
    liftIO $ Prelude.putStrLn ("adding new lobby: " Data.List.++ show newLobbies)
    liftIO $ atomically $ writeTVar (openLobbiesMaster master) newLobbies

    case doesLobbyWithPlayerExist (players_token lobbyToJoin) (lobbynameToJoin lobbyToJoin) newLobbies of
        True -> returnJson lobbyToJoin
        _    -> returnJson LobbyToJoin { lobbynameToJoin="", players_token="" }

doesLobbyWithPlayerExist :: CSRF_Token -> Text -> [Lobby] -> Bool
doesLobbyWithPlayerExist pt lobbynameToSearch lobbies =
    case (Prelude.filter (\l -> lobbynameToSearch == (lobbyname l)) lobbies) of
        []              -> False
        notEmptyList    -> case Prelude.elem pt (player_tokens (Prelude.head notEmptyList)) of
            True -> True
            _    -> False

postLeaveLobbyR :: Handler Value
postLeaveLobbyR = do
    lobbyToLeave <- requireCheckJsonBody :: Handler LobbyToLeave
    master <- getYesod
    openLobbies <- liftIO $ readTVarIO $ openLobbiesMaster master
    let newLobbies = rmPtFromLobbies (leaving_players_token lobbyToLeave) openLobbies
    liftIO $ Prelude.putStrLn ("leaving lobby: " Data.List.++ show lobbyToLeave)
    liftIO $ atomically $ writeTVar (openLobbiesMaster master) newLobbies

    case doesLobbyWithPlayerExist (leaving_players_token lobbyToLeave) (lobbynameToLeave lobbyToLeave) newLobbies of
        True -> returnJson LobbyToLeave { lobbynameToLeave="", leaving_players_token="" }
        _    -> returnJson lobbyToLeave
