{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Game where
import Data.Aeson
import Data.Maybe
import GHC.Generics
import Import hiding (map, head, elem, fst, snd, filter, atomically, readTVarIO, writeTVar, (++), (.))
import Text.Julius (RawJS (..))
import Controller.Actions
import Control.Concurrent.STM
import Control.Lens
import Model.Board
import Model.LobbyModel
import Prelude
import qualified Data.Text.Encoding as TE

data MoveData = MoveData {colorString :: String, fieldNr :: Int} deriving (Generic, Show)
instance ToJSON MoveData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON MoveData

moveDataToFigure :: MoveData -> Figure
moveDataToFigure (MoveData colorStr fieldNr) = Figure (stringToColor colorStr) (intToField fieldNr)

getGameR :: String -> Handler Html
getGameR gameID = do
    master <- getYesod
    gameList <- liftIO $ readTVarIO $ games master
    csrfToken <- lookupCookie $ TE.decodeUtf8 defaultCsrfCookieName :: Handler (Maybe Text)
    if elem gameID (map _lobbyId gameList)
        then do
            newDice <- liftIO $ rollTheDice
            let gameInfo = head $ filter (\x -> _lobbyId x == gameID) gameList :: GameInfo
                oldBS = gameInfo^.boardState :: BoardState
                figs = oldBS^.figures
                isTurnOfClient = isClientsTurn csrfToken gameInfo
            if oldBS^.diceResult == 0 && isTurnOfClient
                then liftIO $ atomically (
                        do
                        gameList2 <- readTVar $ games master
                        let newGameInfo = set (boardState.diceResult) newDice gameInfo
                            newGameList = (:) newGameInfo (filter (\x -> _lobbyId x /= gameID) gameList2)
                        writeTVar (games master) newGameList
                        )
                else do liftIO $ Prelude.putStrLn "Spieler nicht am Zug"
            defaultLayout $ do
                let (hasToDice, diceRes) = if oldBS^.diceResult == 0 
                                            then (True, show newDice) 
                                            else (False, show (oldBS^.diceResult))
                    ownColor = snd $ head $ filter (\x -> fst x == (fromJust csrfToken)) (gameInfo^.colorMap)
                setTitle "Mensch ärgere Dich nicht"
                $(widgetFile "gamepage")
        else 
            defaultLayout $ do
                setTitle "Mensch ärgere Dich nicht"
                $(widgetFile "waitpage")

postGameR :: String -> Handler Value
postGameR gameID = do
    moveData <- requireCheckJsonBody :: Handler MoveData
    master <- getYesod
    gameList <- liftIO $ readTVarIO $ games master
    csrfToken <- lookupCookie $ TE.decodeUtf8 defaultCsrfCookieName :: Handler (Maybe Text)
    if elem gameID (map _lobbyId gameList)
        then let gameInfo = head $ filter (\x -> _lobbyId x == gameID) gameList :: GameInfo
             in if isClientsTurn csrfToken gameInfo
                  then do 
                    let oldBS = _boardState gameInfo :: BoardState
                        newBS = moveFigure (moveDataToFigure moveData) oldBS
                    liftIO $ atomically (
                        do
                        gameList2 <- readTVar $ games master
                        let newGameList = if isGameOver $ newBS^.figures
                            then filter (\x -> _lobbyId x /= gameID) gameList2 
                            else (:) (set boardState newBS gameInfo) (filter (\x -> _lobbyId x /= gameID) gameList2)
                        writeTVar (games master) newGameList)
                    returnJson moveData
                  else returnJson moveData
        else returnJson moveData

isClientsTurn :: (Maybe Text) -> GameInfo -> Bool
isClientsTurn Nothing      _                                = False
isClientsTurn (Just token) (GameInfo _ boardState1 colorMap1) = 
    let turnToken = fst $ head $ filter (\x -> snd x == _turn boardState1) colorMap1
    in turnToken == token
