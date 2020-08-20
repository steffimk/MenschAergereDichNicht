module Actions where

import Prelude

rollTheDice :: Int
rollTheDice = 3 -- TODO

moveFigure :: Figure -> Int -> BoardState -> BoardState
moveFigure figure count boardState
  | !(isValidAction figure count boardState) = boardState
  | otherwise = do
      let homeOffset = getHomeOffsetOfColor (turn boardState)
      let currentFieldNumber = fieldNumber (currentField figure) 
      case (fieldType (currentField figure)) of
        Standard -> newBoardState figure (Figure (number figure) (color Figure) (Field Standard (currentFieldNumber+count))) boardState -- TODO: check if entering any Home Fields, own Home Fields or modulo in general
        Start -> newBoardState figure (Figure (number figure) (color Figure) (Field First (getBoardOffsetOfColor (turn boardState)))) boardState
        Home -> newBoardState figure (Figure (number figure) (color Figure) (Field Home (currentField+count))) boardState -- TODO: if valid, then just add count 
        First -> newBoardState figure (Figure (number figure) (color Figure) (Field Standard (currentFieldNumber+count))) boardState

isValidAction :: Figure -> Int -> BoardState -> Bool
isValidAction figure count boardState
  | (turn boardState) != (color figure)                                       = False
  | hasToMoveFromFirst boardState && fieldType (currentField figure) != First = False
  -- TDOO: | count == 6: Noch Figur in Start und keine in Home 
  -- TODO: | Figur eigener Farbe im Weg, geht nicht weil Home nicht richtig getroffen wird

hasToMoveFromFirst :: BoardState -> Bool
hasToMoveFromFirst boardState = do
  let fieldTypesOfFigures = map (\x -> fieldType (currentField x)) (getFiguresOfColor (figures boardState) (turn boardState))
  if elem First fieldTypesOfFigures && elem Start fieldTypesOfFigures
    then True
    else False