{-# LANGUAGE TemplateHaskell   #-}
module Model.Board where

import Prelude
import Control.Lens

data Color = Yellow | Green | Blue | Red deriving (Eq, Enum)

instance Show Color where
  show Yellow = "Y"
  show Green = "G"
  show Blue = "B"
  show Red = "R"
data Figure = Figure {
                _color  :: Color,
                _currentField :: Field
              } deriving (Eq, Show)

data Field = Standard Int | Start | Home Int | First Int deriving (Eq, Show)

data BoardState = BoardState {
              _figures    :: [Figure],
              _turn       :: Color,
              _diceResult :: Int
      } deriving (Eq, Show)

$(makeLenses ''Figure)
$(makeLenses ''Model.Board.Field)
$(makeLenses ''BoardState)

initNewBoardState :: BoardState
initNewBoardState = BoardState ((initFigures Yellow)++(initFigures Green)++(initFigures Blue)++(initFigures Red)) Yellow 0

initFigures :: Color -> [Figure]
initFigures color1 = (Figure color1 (First (getBoardOffset color1))) : [Figure color1 Start | _ <- [0..2]]

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
  | x < 0           = Start
  | x == 0          = First x
  | x == 14         = First x
  | x == 28         = First x
  | x == 42         = First x
  | elem x [10..13] = Home x
  | elem x [24..27] = Home x
  | elem x [38..41] = Home x
  | elem x [52..55] = Home x
  | otherwise       = Standard x

setToFreshTurn :: BoardState -> BoardState
setToFreshTurn boardState = set turn (nextTurn boardState) (set diceResult 0 boardState)

nextTurn :: BoardState -> Color
nextTurn (BoardState _        color1 6) = color1   
nextTurn (BoardState figures1 Yellow _) = getNextColor figures1 [Green ..]
nextTurn (BoardState figures1 Green  _) = getNextColor figures1 [Blue, Red, Yellow]
nextTurn (BoardState figures1 Blue   _) = getNextColor figures1 [Red, Yellow, Green]
nextTurn (BoardState figures1 Red    _) = getNextColor figures1 [Yellow, Green, Blue]

getNextColor :: [Figure] -> [Color] -> Color
getNextColor figures1 colors =
  let colorsInGame = [ c | c <- colors, (hasFinishedGame figures1 c) == False]
  in if null colorsInGame
       then Yellow -- TODO: Game Ended
       else head colorsInGame

hasFinishedGame :: [Figure] -> Color -> Bool
hasFinishedGame figures1 color1 = foldl (&&) True (map isFigureHome [f | f <- figures1, (_color f) == color1])

isFigureHome :: Figure -> Bool
isFigureHome (Figure _ (Home _)) = True
isFigureHome _                   = False

showGerman :: Color -> String
showGerman Yellow = "Gelb"
showGerman Green = "Grün"
showGerman Blue = "Blau"
showGerman Red = "Rot"

stringToColor :: String -> Color
stringToColor str = 
  case (str!!0) of
    'Y' -> Yellow
    'G' -> Green
    'B' -> Blue
    _   -> Red

getGUIString :: Int -> [Figure] -> String
getGUIString n figures1
  | n    >= 0 = getGUIStringForFieldNumber n figures1
  | n+15 >= 0 = getGUIStringForStartField Yellow n figures1
  | n+25 >= 0 = getGUIStringForStartField Green n figures1
  | n+35 >= 0 = getGUIStringForStartField Blue n figures1
  | otherwise = getGUIStringForStartField Red n figures1

getGUIStringForStartField :: Color -> Int -> [Figure] -> String
getGUIStringForStartField color1 startFieldNumber figures1 =
    if countOf (Figure color1 Start) figures1 > (mod (abs startFieldNumber) 10)
      then (show color1)
      else "___"

getGUIStringForFieldNumber :: Int -> [Figure] -> String
getGUIStringForFieldNumber _ [] = "___" 
getGUIStringForFieldNumber n ((Figure color1 field):xs) = 
  if (getFieldNumber field) == n
    then (show color1)
    else getGUIStringForFieldNumber n xs

countOf :: Eq a => a -> [a] -> Int
countOf x xs = length $ filter (==x) xs