module Controller.Actions where

import Prelude
import Data.Maybe
import Model.Board
import System.Random

rollTheDice :: IO Int
rollTheDice = do
  randInt <- randomIO
  return ((mod randInt 6) + 1)

moveFigure :: Figure -> Int -> BoardState -> BoardState
moveFigure figure count boardState =
  if not ((isTurnOfColor (color figure) (turn boardState)) && (elem figure (figures boardState))
    && (isValidAction figure count boardState))
    then boardState -- TODO: einfach unverändert zurückgeben oder error "Invalid move"
    else let mNewField = getNewField figure count boardState in
      case mNewField of
        Nothing -> boardState -- TODO: einfach unverändert zurückgeben oder error "Invalid move"
        Just _  -> newBoardState (fromJust mNewField) figure boardState

newBoardState :: Field -> Figure -> BoardState -> BoardState 
newBoardState newField (Figure _ oldField) (BoardState figures1 turn1) = 
  let newBoardState1 = BoardState ((delete (Figure turn1 oldField) figures1) ++ [Figure turn1 newField]) (nextTurn figures1 turn1) 
  in if not (mHitColor == Nothing)
        then BoardState ((delete (Figure (fromJust mHitColor) newField) (figures newBoardState1)) ++ [Figure (fromJust mHitColor) Start]) (turn newBoardState1) 
        else newBoardState1
  where mHitColor = getHitColor newField turn1 figures1

isValidAction :: Figure -> Int -> BoardState -> Bool
-- check if valid when 6 got diced
isValidAction (Figure color1 currentField1) 6 (BoardState figures1 _) =
  if isInsertingNewFigureIfNeeded (Figure color1 currentField1) figures1
    then True
    else isMovingFirstIfNeeded (Figure color1 currentField1) figures1
-- cannot move Start figure if count not six
isValidAction (Figure _ Start) _ _ = False
-- other moves
isValidAction (Figure color1 currentField1) _ (BoardState figures1 _) =
  if not (isMovingFirstIfNeeded (Figure color1 currentField1) figures1)
    then False
    else True

-- Ist Farbe, die ziehen will, am Zug
isTurnOfColor :: Color -> Color -> Bool
isTurnOfColor color1 turn1 = turn1 == color1

-- Wenn First von eigener Figur belegt und noch mind 1 Figur im Start: Muss Figur von First wegbewegen
isMovingFirstIfNeeded :: Figure -> [Figure] -> Bool
isMovingFirstIfNeeded (Figure color1 (First x)) figures1 = 
  if x == getBoardOffset color1
    then True
    else not (elem (Figure color1 (First (getBoardOffset color1))) figures1 && (isStartNotEmpty color1 figures1))
isMovingFirstIfNeeded (Figure color1 _) figures1 = not (elem (Figure color1 (First (getBoardOffset color1))) figures1 && (isStartNotEmpty color1 figures1))

-- Wenn sechs gewürfelt und noch mind 1 Figur im Start: Figur aufs Spielbrett bewegen
isInsertingNewFigureIfNeeded :: Figure -> [Figure] -> Bool
isInsertingNewFigureIfNeeded (Figure _ (Start))    _        = True
isInsertingNewFigureIfNeeded (Figure color1 _)     figures1 = not (isStartNotEmpty color1 figures1)

isStartNotEmpty :: Color -> [Figure] -> Bool
isStartNotEmpty color1 figures1 = elem (Figure color1 Start) figures1

-- Neues Feld falls ausführbar (Keine eigene Figur auf dem Feld und Homefeld richtig getroffen)
getNewField :: Figure -> Int -> BoardState -> Maybe Field
getNewField figure count (BoardState figures1 turn1) =
  let mNewField = (calculateNewField figure count) in
    case mNewField of
      Nothing -> Nothing
      Just _  -> if isOccupiedByOwnFigure (fromJust mNewField) turn1 figures1
                    then Nothing
                    else mNewField

isOccupiedByOwnFigure :: Field -> Color -> [Figure] -> Bool
isOccupiedByOwnFigure Start    _      _        = False
isOccupiedByOwnFigure newField color1 figures1 = elem (Figure color1 newField) figures1

getHitColor :: Field -> Color -> [Figure] -> Maybe Color
getHitColor Start    _      _       = Nothing
getHitColor (Home _) _      _       = Nothing
getHitColor newField turn1 figures1   = 
  let hitList = [ color x | x <- figures1, currentField x == newField, color x /= turn1]
  in if hitList == []
      then Nothing
      else Just (hitList!!0)

calculateNewField :: Figure -> Int -> Maybe Field
calculateNewField (Figure color1 Start)         _      = Just (First (getBoardOffset color1))
calculateNewField (Figure color1 (First x))     count1 = 
  if x == getBoardOffset color1
    then Just (intToField (x + count1))
    else calculateField color1 x count1
calculateNewField (Figure color1 currentField1) count1 = calculateField color1 (getFieldNumber currentField1) count1
    
calcHelper :: Int -> Int -> [Int] -> Maybe Field
calcHelper x count1 overShoot =
  let newNumber = (x + count1)
    in if elem (overShoot!!0) [x..newNumber]
        then Nothing
        else if elem (overShoot!!1) [x..newNumber] || elem (overShoot!!2) [x..newNumber] || elem (overShoot!!3) [x..newNumber]
              then Just (intToField $ mod (newNumber+4) 56)
              else Just (intToField $ mod newNumber 56)

calculateField :: Color -> Int -> Int -> Maybe Field
calculateField Yellow x count1 = calcHelper x count1 [56, 10, 24, 38]
calculateField Green  x count1 = calcHelper x count1 [14, 24, 38, 52]
calculateField Blue   x count1 = calcHelper x count1 [28, 10, 38, 52]
calculateField Red    x count1 = calcHelper x count1 [42, 10, 24, 52]

delete :: Eq t => t -> [t] -> [t]
delete _ []    = []
delete e (x:xs) 
  | e == x    = xs
  | otherwise = x : (delete e xs)
