{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Lobby where

import           Control.Monad.State
import           Data.Aeson
import           Data.List
import           Import
import           Model.Board
import           Model.LobbyModel
import           Prelude             hiding ((++))
import           Text.Shakespeare

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
    openGames <- liftIO $ readTVarIO $ games master
    let player_token = (Prelude.head (player_tokens lobby))
        lobbyToJoin = lobbyname lobby
        openLobbiesPlayerRemoved = addPtToLobbyAndRemoveFromOthers player_token lobbyToJoin openLobbies openGames
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
    where rmPt player_token lobby = case (Prelude.length (player_tokens lobby) >= 4) of
                                                    True -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(player_tokens lobby) }
                                                    _    -> Lobby { lobbyname=(lobbyname lobby), player_tokens=(Data.List.delete player_token (player_tokens lobby)) }

addPtToLobbyAndRemoveFromOthers :: CSRF_Token -> Text -> [Lobby] -> [GameInfo] -> [Lobby]
addPtToLobbyAndRemoveFromOthers pt ln lobbies openGames =
    case doesGameWithPlayerExist (Just pt) openGames of
        True -> Prelude.filter (\l -> (player_tokens l) /= []) lobbies
        _    -> rmEmptyLobbies (Prelude.map (\l -> addPt pt ln l) lobbies)
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
    openGames <- liftIO $ readTVarIO $ games master
    token <- lookupCookie "XSRF-TOKEN"

    -- if doesGameWithPlayerExist token openGames
    --     then returnJson LobbyToJoin { lobbynameToJoin="", players_token="" }
    --     else do

    let newLobbies = addPtToLobbyAndRemoveFromOthers (players_token lobbyToJoin) (lobbynameToJoin lobbyToJoin) openLobbies openGames

    case (Data.List.find (\l -> (lobbyname l)==(lobbynameToJoin lobbyToJoin)) newLobbies) of
        Nothing -> liftIO $ atomically $ writeTVar (openLobbiesMaster master) newLobbies
        Just lobby -> case shouldLobbyBeConvertedToGame lobby of
            False -> liftIO $ atomically $ writeTVar (openLobbiesMaster master) newLobbies
            True  -> let lobbies = (Data.List.filter (\l -> (lobbyname l)/=(lobbynameToJoin lobbyToJoin)) newLobbies)
                         newOpenGame = convertLobbyToGame (Prelude.head (Data.List.filter (\l -> (lobbyname l)==(lobbynameToJoin lobbyToJoin)) newLobbies))
                     in do { liftIO $ atomically $ writeTVar (openLobbiesMaster master) lobbies;
                             liftIO $ atomically $ writeTVar (games master) (openGames Data.List.++ [newOpenGame]) }

    case (doesLobbyWithPlayerExist (players_token lobbyToJoin) (lobbynameToJoin lobbyToJoin) newLobbies) || (not (doesGameWithPlayerExist token openGames)) of
        True -> returnJson lobbyToJoin
        _    -> returnJson LobbyToJoin { lobbynameToJoin="", players_token="" }

-- getLobbyFromMaybeLobby :: Maybe Lobby -> Lobby


isMaybeLobbyFull :: (Maybe Lobby) -> Bool
isMaybeLobbyFull maybeLobby = do
    case maybeLobby of
        Just _ -> True
        _      -> False

shouldLobbyBeConvertedToGame :: Lobby -> Bool
shouldLobbyBeConvertedToGame lobby =
    case (Prelude.length (player_tokens lobby) >= 4) of
        False -> False
        True  -> True

convertLobbyToGame :: Lobby -> GameInfo
convertLobbyToGame oldLobby  =
    let colorList = [Red, Blue, Yellow, Green]
    in GameInfo { _lobbyId=(unpack (lobbyname oldLobby)), _boardState=initNewBoardState, _colorMap=(Prelude.zip (player_tokens oldLobby) colorList) }


doesLobbyWithPlayerExist :: CSRF_Token -> Text -> [Lobby] -> Bool
doesLobbyWithPlayerExist pt lobbynameToSearch lobbies =
    case (Prelude.filter (\l -> lobbynameToSearch == (lobbyname l)) lobbies) of
        []              -> False
        notEmptyList    -> case Prelude.elem pt (player_tokens (Prelude.head notEmptyList)) of
            True -> True
            _    -> False

doesGameWithPlayerExist :: Maybe CSRF_Token -> [GameInfo] -> Bool
doesGameWithPlayerExist maybeToken gamesList =
    case maybeToken of
        Nothing     -> False
        Just csrf   -> case (Prelude.filter (\game -> playerTokenInColorMap (csrf :: CSRF_Token) (_colorMap game)) gamesList) of
                            [] -> False
                            _  -> True

postLeaveLobbyR :: Handler Value
postLeaveLobbyR = do
    lobbyToLeave <- requireCheckJsonBody :: Handler LobbyToLeave
    master <- getYesod
    openLobbies <- liftIO $ readTVarIO $ openLobbiesMaster master
    openGames <- liftIO $ readTVarIO $ games master
    token <- lookupCookie "XSRF-TOKEN"
    let newLobbies = rmPtFromLobbies (leaving_players_token lobbyToLeave) openLobbies
    liftIO $ Prelude.putStrLn ("leaving lobby: " Data.List.++ show lobbyToLeave)
    liftIO $ atomically $ writeTVar (openLobbiesMaster master) newLobbies

    case doesLobbyWithPlayerExist (leaving_players_token lobbyToLeave) (lobbynameToLeave lobbyToLeave) newLobbies of
        True -> returnJson LobbyToLeave { lobbynameToLeave="", leaving_players_token="" }
        _    -> case playersGameId token openGames of
                    Just _  -> returnJson LobbyToLeave { lobbynameToLeave="", leaving_players_token="" }
                    Nothing -> returnJson lobbyToLeave

getLobbyOrGameExistsR :: String -> Handler Value
getLobbyOrGameExistsR lnToCheckString = do
    master <- getYesod
    openLobbies <- liftIO $ readTVarIO $ openLobbiesMaster master
    openGames <- liftIO $ readTVarIO $ games master

    let lnToCheckText = pack' lnToCheckString
        getLnFromOpenLobbies lobby = (lobbyname lobby)
        getLnFromGames game = (_lobbyId game)
        in returnJson ((Prelude.elem lnToCheckText (Prelude.map getLnFromOpenLobbies openLobbies) || Prelude.elem lnToCheckString (Prelude.map getLnFromGames openGames)))

getPlayersGameIdR :: Handler Value
getPlayersGameIdR = do
    master <- getYesod
    token <- lookupCookie "XSRF-TOKEN"
    openGames <- liftIO $ readTVarIO $ games master

    case playersGameId token openGames of
        Nothing     -> returnJson ("" :: Text)
        Just gameId -> returnJson gameId

playersGameId :: Maybe CSRF_Token -> [GameInfo] -> Maybe Text
playersGameId maybeToken gamesList =
    case maybeToken of
        Nothing     -> Nothing
        Just csrf   -> case (Prelude.filter (\game -> playerTokenInColorMap (csrf :: CSRF_Token) (_colorMap game)) gamesList) of
                            []     -> Nothing
                            game:_ -> Just (pack (_lobbyId game))

playerTokenInColorMap :: CSRF_Token -> [(CSRF_Token, Color)] -> Bool
playerTokenInColorMap pt cm =
    case (Data.List.findIndex ((pt==) Prelude.. fst) cm) of
        Nothing -> False
        Just _  -> True
