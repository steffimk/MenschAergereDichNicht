module Board where

import Prelude

data Color = Yellow | Green | Blue | Red deriving (Eq, Enum, Show)

data Figure = Figure {
                color  :: Color,
                currentField :: Field
              } deriving Show

data Field = Standard Int | Start | Home Int | First Int deriving (Eq, Show)

data BoardState = BoardState {
              figures :: [Figure],
              turn        :: Color
      } deriving Show

initNewBoardState :: BoardState
initNewBoardState = BoardState ((initFigures Yellow)++(initFigures Green)++(initFigures Blue)++(initFigures Red)) Yellow -- TODO: immer gelb?

initFigures :: Color -> [Figure]
initFigures color = [Figure color Start | x <- [0..3]]

getBoardOffset :: Color -> Int
getBoardOffset color = 
  case color of
    Yellow  -> 0
    Green   -> 14
    Blue    -> 28
    _       -> 42

getFieldNumber :: Field -> Int
getFieldNumber Start        = -1
getFieldNumber (Standard x) = x
getFieldNumber (Home x)     = x
getFieldNumber (First x)    = x

getHomeOffset :: Color -> Int
getHomeOffset color =
  case color of
    Yellow  -> 52
    Green   -> 10
    Blue    -> 24
    _       -> 38

intToField :: Int -> Field
intToField x
  | x == 0          = First x
  | x == 14         = First x
  | x == 28         = First x
  | x == 42         = First x
  | elem x [10..13] = Home x
  | elem x [24..27] = Home x
  | elem x [38..41] = Home x
  | elem x [52..55] = Home x
  | otherwise       = Standard x
