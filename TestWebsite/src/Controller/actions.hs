module Actions where

import Prelude

rollTheDice RandomGen g => Int -> g -> [Word]
rollTheDice n = take n . unfoldr (Just . uniformR (1, 6))

moveFigure :: Figure -> Int -> BoardState -> BoardState
moveFigure figure count boardState
  | !(isValidAction figure count boardState) = boardState
  | otherwise = do
      let currentFieldNumber = fieldNumber (currentField figure)
      case (fieldType (currentField figure)) of
        Standard x -> newBoardState figure (Figure (number figure) (color Figure) (getNewField (color figure) currentFieldNumber count)) boardState -- TODO: check if entering any Home Fields, own Home Fields or modulo in general
        Start -> newBoardState figure (Figure (number figure) (color Figure) (First (getBoardOffsetOfColor (turn boardState)))) boardState
        Home x -> newBoardState figure (Figure (number figure) (color Figure) (Home (currentField+count))) boardState -- TODO: if valid, then just add count 
        First x -> newBoardState figure (Figure (number figure) (color Figure) (Standard (currentFieldNumber+count))) boardState
        -- TODO: Prüfen ob Figur rausgeworfen wurde

isValidAction :: Figure -> Int -> BoardState -> Bool
isValidAction (Figure color currentField) count (BoardState figures turn)
-- Wenn Farbe ziehen will, die nicht dran ist
  | color != turn   = False
-- Wenn First von eigener Figur belegt und noch mind 1 Figur im Start: Muss Figur von First wegbewegen
  | elem (Figure color (First (getBoardOffsetOfColor color))) figures && (isStartNotEmpty color)
    = do case currentField of 
                First x   -> True
                otherwise -> False
-- Wenn sechs gewürfelt und noch mind 1 Figur im Start: Figur aufs Spielbrett bewegen
  | count == 6 && (isStartNotEmpty color) = do
    if currentField == Start
      then True
      else False
  | if (getNewField color ) -- TODOOOO
  -- TODO: | Figur eigener Farbe im Weg, geht nicht weil Home nicht richtig getroffen wird

isStartNotEmpty :: Color -> BoardState -> Bool
isStartNotEmpty color (BoardState figures _) = elem (Figure color Start) figures

getNewFieldNumber :: Color -> Int -> Int -> Int
getNewFieldNumber color fieldNumber count = do
  let homeOffset = getHomeOffsetOfColor color
  if (fieldNumber + count) >= homeOffset && (fieldNumber + count) < (homeOffset + 4) -- TODO