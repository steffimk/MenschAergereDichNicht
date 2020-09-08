module Model.Board where

import Prelude

data Color = Yellow | Green | Blue | Red deriving (Eq, Enum)

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

instance Show Color where
  show Yellow = "Y"
  show Green = "G"
  show Blue = "B"
  show Red = "R"

showGerman :: Color -> String
showGerman Yellow = "Gelb"
showGerman Green = "Grün"
showGerman Blue = "Blau"
showGerman Red = "Rot"

stringToColor :: String -> Color
stringToColor "Y" = Yellow
stringToColor "G" = Green
stringToColor "B" = Blue
stringToColor _   = Red

getGUIString :: Int -> [Figure] -> String
getGUIString n figures1
  | n >= 0 = getGUIStringForFieldNumber n figures1
  | n == -10 = if countOf (Figure Yellow Start) figures1 > 0
                then "Y"
                else "___"
  | n == -11 = if countOf (Figure Yellow Start) figures1 > 1
                then "Y"
                else "___"
  | n == -12 =if countOf (Figure Yellow Start) figures1 > 2
                then "Y"
                else "___"
  | n == -13 = if countOf (Figure Yellow Start) figures1 > 3
                then "Y"
                else "___"
  | n == -20 = if countOf (Figure Green Start) figures1 > 0
                then "G"
                else "___"
  | n == -21 = if countOf (Figure Green Start) figures1 > 1
                then "G"
                else "___"
  | n == -22 = if countOf (Figure Green Start) figures1 > 2
                then "G"
                else "___"
  | n == -23 = if countOf (Figure Green Start) figures1 > 3
                then "G"
                else "___"
  | n == -30 = if countOf (Figure Blue Start) figures1 > 0
                then "B"
                else "___"
  | n == -31 = if countOf (Figure Blue Start) figures1 > 1
                then "B"
                else "___"
  | n == -32 = if countOf (Figure Blue Start) figures1 > 2
                then "B"
                else "___"
  | n == -33 = if countOf (Figure Blue Start) figures1 > 3
                then "B"
                else "___"
  | n == -40 = if countOf (Figure Red Start) figures1 > 0
                then "R"
                else "___"
  | n == -41 = if countOf (Figure Red Start) figures1 > 1
                then "R"
                else "___"
  | n == -42 = if countOf (Figure Red Start) figures1 > 2
                then "R"
                else "___"
  | n == -43 = if countOf (Figure Red Start) figures1 > 3
                then "R"
                else "___"
  

getGUIStringForFieldNumber :: Int -> [Figure] -> String
getGUIStringForFieldNumber _ [] = "___" 
getGUIStringForFieldNumber n ((Figure color1 field):xs) = 
  if (getFieldNumber field) == n
    then (show color1)
    else getGUIStringForFieldNumber n xs

countOf :: Eq a => a -> [a] -> Int
countOf x xs = length $ filter (==x) xs