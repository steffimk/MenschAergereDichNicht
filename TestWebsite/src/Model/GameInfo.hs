module Model.GameInfo where

import Model.Board

data GameInfo = GameInfo {lobbyId :: String, boardState :: BoardState, colorMap :: [(String, Color)]}
