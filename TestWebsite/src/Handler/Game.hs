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
    if elem gameID (map fst gameList)
        then
            do
                let boardState = snd $ head $ filter (\x -> fst x == gameID) gameList :: BoardState
                    figs = figures boardState
                    color = turn boardState
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
    if elem gameID (map fst gameList)
        then do
            dice <- liftIO $ rollTheDice
            let oldBoardState = snd $ head $ filter (\x -> fst x == gameID) gameList :: BoardState
                diceResult = dice -- TODO: dice boardState 
                boardState = moveFigure (moveDataToFigure moveData) diceResult oldBoardState
            liftIO $ Prelude.putStrLn (show boardState)
            liftIO $ Prelude.putStrLn ("###############" ++ (show dice))
            let newGameList = (:) (gameID, boardState) (filter (\x -> fst x /= gameID) gameList) 
            liftIO $ atomically $ writeTVar (games master) newGameList
            returnJson moveData
        else returnJson moveData
    -- defaultLayout $ do
    --     let move = show $ moveData
    --     setTitle "Mensch ärgere Dich nicht"
    --     $(widgetFile "waitpage")