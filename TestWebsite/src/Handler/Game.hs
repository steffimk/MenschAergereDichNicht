{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Game where
import Data.Aeson
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
    gameList <- liftIO $ Control.Concurrent.STM.readTVarIO $ games master
    csrfToken <- lookupCookie $ TE.decodeUtf8 defaultCsrfCookieName :: Handler (Maybe Text)
    if elem gameID (map _lobbyId gameList)
        then do
            newDice <- liftIO $ rollTheDice
            let gameInfo = head $ filter (\x -> _lobbyId x == gameID) gameList :: GameInfo
                oldBS = gameInfo^.boardState :: BoardState
                figs = oldBS^.figures
                isTurnOfClient = isClientsTurn csrfToken gameInfo
            if oldBS^.diceResult == 0 && isTurnOfClient
                then let newGameInfo = set (boardState.diceResult) newDice gameInfo
                         newGameList = (:) newGameInfo (filter (\x -> _lobbyId x /= gameID) gameList)
                     in do liftIO $ atomically $ writeTVar (games master) newGameList
                else do liftIO $ Prelude.putStrLn "Spieler nicht am Zug"
            defaultLayout $ do
                let diceRes = if oldBS^.diceResult == 0 
                                then if isTurnOfClient then show newDice else "___"
                                else show (oldBS^.diceResult)
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
        then do
            let gameInfo = head $ filter (\x -> _lobbyId x == gameID) gameList :: GameInfo
                oldBS = _boardState gameInfo :: BoardState
                newBS = if (isClientsTurn csrfToken gameInfo)
                            then moveFigure (moveDataToFigure moveData) oldBS
                            else oldBS
                -- newBS = moveFigure (moveDataToFigure moveData) oldBS :: BoardState
            liftIO $ Prelude.putStrLn (show newBS)
            let newGameList = if isGameOver $ newBS^.figures
                    then filter (\x -> _lobbyId x /= gameID) gameList 
                    else (:) (set boardState newBS gameInfo) (filter (\x -> _lobbyId x /= gameID) gameList) 
            liftIO $ atomically $ writeTVar (games master) newGameList
            returnJson moveData
        else returnJson moveData

isClientsTurn :: (Maybe Text) -> GameInfo -> Bool
isClientsTurn Nothing      _                                = False
isClientsTurn (Just token) (GameInfo _ boardState1 colorMap1) = 
    let turnToken = fst $ head $ filter (\x -> snd x == _turn boardState1) colorMap1
    in turnToken == token

-- if isGameOver $ gameInfo^.boardState.figures
--     then do
--         liftIO $ atomically $ writeTVar $ filter (\x -> _lobbyId x /= gameID) gameList
--         defaultLayout $ do
--             setTitle "Mensch ärgere Dich nicht"
--             $(widgetFile "waitpage")