module Board where

import Prelude

data Color = Yellow | Green | Blue | Red deriving Show

data FieldType = Standard | Start | Home | First deriving Show

data Figure = Figure {
                number :: Int,
                color  :: Color
              } deriving Show

data Field = Field {
              fieldType   :: FieldType,
              fieldColor  :: Maybe Color,
              figure      :: Maybe Figure
            } deriving Show

data StartFields = StartFields {
              yellowHome :: [Field],
              greenHome  :: [Field],
              blueHome   :: [Field],
              redHome    :: [Field]
            } deriving Show

data BoardState = BoardState {
                    board      :: [Field],
                    startFields :: StartFields
                  } deriving Show

initNewBoardState :: BoardState
initNewBoardState = do
    let startFields = StartFields (initStart Yellow) (initStart Green) (initStart Blue) (initStart Red)
    let board = [ (intToField x) | x <- [0..55] ]
    BoardState{ board = board, startFields = startFields }

intToField :: Int -> Field
intToField x
  | x == 0          = Field First (Just Yellow) Nothing
  | x == 14         = Field First (Just Green) Nothing
  | x == 28         = Field First (Just Blue) Nothing
  | x == 42         = Field First (Just Red) Nothing
  | elem x [10..13] = Field Home (Just Green) Nothing
  | elem x [24..27] = Field Home (Just Blue) Nothing
  | elem x [38..41] = Field Home (Just Red) Nothing
  | elem x [52..55] = Field Home (Just Yellow) Nothing
  | otherwise       = Field Standard Nothing Nothing

initStart :: Color -> [Field]
initStart color = [Field Start (Just color) (Just (Figure 0 color)), Field Start (Just color) (Just (Figure 1 color)),
                  Field Start (Just color) (Just (Figure 2 color)), Field Start (Just color) (Just (Figure 3 color))]