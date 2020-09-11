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
import Import hiding (map, head, elem, fst, snd, filter, atomically, readTVarIO, writeTVar, (++))
import Text.Julius (RawJS (..))
import Controller.Actions
import Control.Concurrent.STM
import Model.Board
import Model.GameInfo
import Prelude

data MoveData = MoveData {colorString :: String, fieldNr :: Int} deriving (Generic, Show)
instance ToJSON MoveData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON MoveData

moveDataToFigure :: MoveData -> Figure
moveDataToFigure (MoveData colorString fieldNr) = Figure (stringToColor colorString) (intToField fieldNr)

getGameR :: String -> Handler Html
getGameR gameID = do
    master <- getYesod
    gameList <- liftIO $ Control.Concurrent.STM.readTVarIO $ games master
    if elem gameID (map lobbyId gameList)
        then do
            newDice <- liftIO $ rollTheDice
            let gameInfo = head $ filter (\x -> lobbyId x == gameID) gameList :: GameInfo
                oldBoardState = boardState gameInfo :: BoardState
                dice = if diceResult oldBoardState > 0 then (diceResult oldBoardState) else newDice
                figs = figures oldBoardState
                color = turn oldBoardState
            if diceResult oldBoardState == 0 
                then let newBoardState = oldBoardState {diceResult = dice}
                         newGameList = (:) (GameInfo gameID newBoardState (colorMap gameInfo)) (filter (\x -> lobbyId x /= gameID) gameList)
                     in do liftIO $ atomically $ writeTVar (games master) newGameList
                else do return ()
            defaultLayout $ do
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
    if elem gameID (map lobbyId gameList)
        then do
            let gameInfo = head $ filter (\x -> lobbyId x == gameID) gameList :: GameInfo
                oldBoardState = boardState gameInfo :: BoardState
                newBoardState = moveFigure (moveDataToFigure moveData) oldBoardState
            liftIO $ Prelude.putStrLn (show newBoardState)
            let newGameList = (:) (GameInfo gameID newBoardState (colorMap gameInfo)) (filter (\x -> lobbyId x /= gameID) gameList) 
            liftIO $ atomically $ writeTVar (games master) newGameList
            returnJson moveData
        else returnJson moveData
    -- defaultLayout $ do
    --     let move = show $ moveData
    --     setTitle "Mensch ärgere Dich nicht"
    --     $(widgetFile "waitpage")