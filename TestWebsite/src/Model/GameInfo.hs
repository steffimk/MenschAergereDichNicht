module Model.GameInfo where

import           Model.Board
import           Model.LobbyModel

data GameInfo = GameInfo {lobbyId :: String, boardState :: BoardState, colorMap :: [(CSRF_Token, Color)]} deriving Show
