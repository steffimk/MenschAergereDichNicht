module Actions where

import Prelude
import Data.Maybe
import Board

-- rollTheDice RandomGen g => Int -> g -> [Word]
-- rollTheDice n = take n . unfoldr (Just . uniformR (1, 6))

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
  let newBoardState = BoardState ((delete (Figure turn1 oldField) figures1) ++ [Figure turn1 newField]) (nextTurn figures1 turn1) 
  in if not (mHitColor == Nothing)
        then BoardState ((delete (Figure (fromJust mHitColor) newField) (figures newBoardState)) ++ [Figure (fromJust mHitColor) Start]) (turn newBoardState) 
        else newBoardState
  where mHitColor = getHitColor newField turn1 figures1

isValidAction :: Figure -> Int -> BoardState -> Bool
-- check if valid when 6 got diced
isValidAction (Figure color1 currentField1) 6 (BoardState figures1 turn1) =
  if not (isMovingFirstIfNeeded (Figure color1 currentField1) figures1)
    then False
    else if isInsertingNewFigureIfNeeded (Figure color1 currentField1) figures1
          then True
          else False
-- cannot move Start figure if count not six
isValidAction (Figure color1 Start) _ _ = False
-- other moves
isValidAction (Figure color1 currentField1) count (BoardState figures1 turn1) =
  if not (isMovingFirstIfNeeded (Figure color1 currentField1) figures1)
    then False
    else True

-- Ist Farbe, die ziehen will, am Zug
isTurnOfColor :: Color -> Color -> Bool
isTurnOfColor color turn = turn == color

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
isStartNotEmpty color figures = elem (Figure color Start) figures

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
isOccupiedByOwnFigure Start    _     _       = False
isOccupiedByOwnFigure newField color figures = elem (Figure color newField) figures

getHitColor :: Field -> Color -> [Figure] -> Maybe Color
getHitColor Start    _      _       = Nothing
getHitColor (Home _) _      _       = Nothing
getHitColor newField turn figures   = 
  let hitList = [ color x | x <- figures, currentField x == newField, color x /= turn]
  in if hitList == []
      then Nothing
      else Just (hitList!!0)

calculateNewField :: Figure -> Int -> Maybe Field
calculateNewField (Figure color1 Start)         _     = Just (First (getBoardOffset color1))
calculateNewField (Figure color1 (First x))     count = Just (Standard ((getBoardOffset color1) + count))
calculateNewField (Figure color1 currentField1) count =
  let calcHelper = \x count overShoot ->
          let newNumber = (x + count)
          in if elem (overShoot!!0) [x..newNumber]
              then Nothing
              else if elem (overShoot!!1) [x..newNumber] || elem (overShoot!!2) [x..newNumber] || elem (overShoot!!3) [x..newNumber]
                    then Just (intToField (newNumber+4))
                    else Just (intToField newNumber)
  in let  calculateField Yellow x count = calcHelper x count [56, 10, 24, 38]
          calculateField Green  x count = calcHelper x count [14, 24, 38, 52]
          calculateField Blue   x count = calcHelper x count [28, 10, 38, 52]
          calculateField Red    x count = calcHelper x count [42, 10, 24, 52]
      in case currentField1 of
          Standard x -> calculateField color1 x count
          Home x     -> calculateField color1 x count

delete _ []    = []
delete e (x:xs) 
  | e == x    = xs
  | otherwise = x : (delete e xs)