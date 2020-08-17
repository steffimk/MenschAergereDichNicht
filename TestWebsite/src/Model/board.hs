module Board where

import Prelude

data Color = Yellow | Green | Blue | Red deriving Show

data FieldType = Standard | Start | Home | First deriving Show

data Figure = Figure {
                number :: Int,
                color  :: Color
              }

data Field = Field {
              fieldType   :: FieldType,
              fieldColor  :: Maybe Color,
              figure      :: Maybe Figure
            }

data StartFields = StartFields {
              yellowHome :: [Field],
              greenHome  :: [Field],
              blueHome   :: [Field],
              redHome    :: [Field]
            }

data BoardStatus = BoardStatus {
                    board      :: [Field],
                    startFields :: StartFields
                  }

let startFields = StartFields{
                    yellowHome = initStart Yellow,
                    greenHome  = initStart Green,
                    blueHome   = initStart Blue,
                    redHome    = initStart Red
                  }

let board = [ (intToField x) | x <- [0..55] ]

let status = BoardStatus{ board = board, homeFields = homeFields }

2
initStart :: Color -> [Field]
initStart color = [Field Start (Just color) (Just (Figure 0 color)), Field Start (Just color) (Just (Figure 1 color)),
                  Field Start (Just color) (Just (Figure 2 color)), Field Start (Just color) (Just (Figure 3 color))]