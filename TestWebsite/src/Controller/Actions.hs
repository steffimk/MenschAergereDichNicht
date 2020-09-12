module Controller.Actions where

import Prelude
import Data.Maybe
import Model.Board
import System.Random
import Control.Lens

rollTheDice :: IO Int
rollTheDice = do
  randInt <- randomIO
  return ((mod randInt 6) + 1)

-- Returns new modified boardState if move is valid, otherwise boardState is returned unchanged
moveFigure :: Figure -> BoardState -> BoardState
moveFigure _ (BoardState figs turn1 0) = BoardState figs turn1 0
moveFigure figure boardState =
  if not ((isTurnOfColor (figure^.color) (boardState^.turn)) && (elem figure (boardState^.figures)))
    then boardState
    else if (isValidAction figure boardState)
      then let mNewField = getNewField figure boardState in
        case mNewField of
          Nothing -> if canMakeAMove boardState then boardState else setToFreshTurn boardState
          Just _  -> newBoardState (fromJust mNewField) figure boardState
      else if canMakeAMove boardState
            then boardState
            else setToFreshTurn boardState

canMakeAMove :: BoardState -> Bool
canMakeAMove boardState = foldl (||) False $ map (\x -> isValidAction x boardState && getNewField x boardState /= Nothing) [f | f <- (_figures boardState), _color f == _turn boardState]

newBoardState :: Field -> Figure -> BoardState ->  BoardState 
newBoardState newField oldFig oldBS = 
  let newBoardState1 = adjustForNextRound (Figure (oldBS^.turn) newField) oldFig (setToFreshTurn oldBS)
  in if not (mHitColor == Nothing)
        then adjustForNextRound (Figure (fromJust mHitColor) Start) (Figure (fromJust mHitColor) newField) newBoardState1
        else newBoardState1
  where mHitColor = getHitColor newField oldBS

adjustForNextRound :: Figure -> Figure -> BoardState -> BoardState
adjustForNextRound toAdd toDelete boardState = 
  over figures (\x -> toAdd : (delete toDelete x)) boardState

isValidAction :: Figure -> BoardState -> Bool
-- check if valid when 6 got diced
isValidAction (Figure color1 currentField1) (BoardState figures1 _ 6) =
  if isInsertingNewFigureIfNeeded (Figure color1 currentField1) figures1
    then True
    else currentField1 == First (getBoardOffset color1)
-- cannot move Start figure if count not six
isValidAction (Figure _ Start) _ = False
-- other moves
isValidAction (Figure color1 currentField1) (BoardState figures1 _ diceRes) =
  if not (isMovingFirstIfNeeded (Figure color1 currentField1) figures1 diceRes)
    then False
    else True

-- Ist Farbe, die ziehen will, am Zug
isTurnOfColor :: Color -> Color -> Bool
isTurnOfColor color1 turn1 = turn1 == color1

-- Wenn First von eigener Figur belegt und noch mind 1 Figur im Start: Muss Figur von First wegbewegen
isMovingFirstIfNeeded :: Figure -> [Figure] -> Int -> Bool
isMovingFirstIfNeeded (Figure color1 (First x)) figures1 diceRes = 
  if x == getBoardOffset color1
    then True
    else not (elem (Figure color1 (First (getBoardOffset color1))) figures1 && (hasStartFigures color1 figures1))
      || elem (Figure color1 (Standard ((getBoardOffset color1)+diceRes))) figures1
isMovingFirstIfNeeded (Figure color1 _) figures1 diceRes = 
  not (elem (Figure color1 (First (getBoardOffset color1))) figures1 && (hasStartFigures color1 figures1))
    || elem (Figure color1 (Standard ((getBoardOffset color1)+diceRes))) figures1

-- Wenn sechs gewürfelt und noch mind 1 Figur im Start: Figur aufs Spielbrett bewegen
isInsertingNewFigureIfNeeded :: Figure -> [Figure] -> Bool
isInsertingNewFigureIfNeeded (Figure _ Start)              _        = True
isInsertingNewFigureIfNeeded (Figure color1 currentField1) figures1 =
  not (hasStartFigures color1 figures1) || currentField1 == First (getBoardOffset color1) 
  || (elem (Figure color1 (Standard ((getBoardOffset color1)+6))) figures1 && elem (Figure color1 (First (getBoardOffset color1))) figures1)

hasStartFigures :: Color -> [Figure] -> Bool
hasStartFigures color1 figures1 = elem (Figure color1 Start) figures1

-- Neues Feld falls ausführbar (Keine eigene Figur auf dem Feld und Homefeld richtig getroffen)
getNewField :: Figure -> BoardState -> Maybe Field
getNewField figure (BoardState figures1 turn1 diceRes) =
  let mNewField = (calculateNewField figure diceRes) in
    case mNewField of
      Nothing -> Nothing
      Just _  -> if isOccupiedByOwnFigure (fromJust mNewField) turn1 figures1
                    then Nothing
                    else mNewField

isOccupiedByOwnFigure :: Field -> Color -> [Figure] -> Bool
isOccupiedByOwnFigure Start    _      _        = False
isOccupiedByOwnFigure newField color1 figures1 = elem (Figure color1 newField) figures1

getHitColor :: Field -> BoardState -> Maybe Color
getHitColor Start    _     = Nothing
getHitColor (Home _) _     = Nothing
getHitColor newField bs    = 
  let hitList =  [ x^.color | x <- bs^.figures, x^.currentField == newField, x^.color /= bs^.turn]
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
