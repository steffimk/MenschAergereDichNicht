module Model.Board where

import Prelude

data Color = Yellow | Green | Blue | Red deriving (Eq, Enum, Show)

data Figure = Figure {
                color  :: Color,
                currentField :: Field
              } deriving (Eq, Show)

data Field = Standard Int | Start | Home Int | First Int deriving (Eq, Show)

data BoardState = BoardState {
              figures :: [Figure],
              turn        :: Color
      } deriving Show

initNewBoardState :: BoardState
initNewBoardState = BoardState ((initFigures Yellow)++(initFigures Green)++(initFigures Blue)++(initFigures Red)) Yellow -- TODO: immer gelb?

initFigures :: Color -> [Figure]
initFigures color1 = [Figure color1 Start | x <- [0..3]]

getBoardOffset :: Color -> Int
getBoardOffset color1 = 
  case color1 of
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
getHomeOffset color1 =
  case color1 of
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

nextTurn :: [Figure] -> Color -> Color
nextTurn figures1 Yellow = getNextColor figures1 [Green ..]
nextTurn figures1 Green  = getNextColor figures1 [Blue, Red, Yellow]
nextTurn figures1 Blue  = getNextColor figures1 [Red, Yellow, Green]
nextTurn figures1 Red  = getNextColor figures1 [Yellow, Green, Blue]

getNextColor :: [Figure] -> [Color] -> Color
getNextColor figures1 colors =
  let colorsInGame = [ c | c <- colors, (hasFinishedGame figures1 c) == False]
  in if null colorsInGame
       then Yellow -- TODO
       else head colorsInGame

hasFinishedGame :: [Figure] -> Color -> Bool
hasFinishedGame figures1 color1 = foldl (&&) True (map isFigureHome [f | f <- figures1, (color f) == color1])

isFigureHome :: Figure -> Bool
isFigureHome (Figure _ (Home _)) = True
isFigureHome _                   = False
