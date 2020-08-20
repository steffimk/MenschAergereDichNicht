module Board where

import Prelude

data Color = Yellow | Green | Blue | Red deriving (Eq, Enum, Show)

data FieldType = Standard | Start | Home | First deriving (Eq, Enum, Show)

data Figure = Figure {
                number :: Int,
                color  :: Color,
                currentField :: Field
              } deriving Show

data Field = Field {
              fieldType   :: FieldType,
              fieldNumber :: Int
            } deriving Show

data AllFigures = AllFigures {
              yellowFigures :: [Figure],
              greenFigures  :: [Figure],
              blueFigures   :: [Figure],
              redFigures    :: [Figure]
            } deriving Show

data BoardState = BoardState {
        figures     :: AllFigures,
        turn        :: Color
      } deriving Show

-- data StartFields = StartFields {
--               yellowHome :: [Field],
--               greenHome  :: [Field],
--               blueHome   :: [Field],
--               redHome    :: [Field]
--             } deriving Show

initNewBoardState :: BoardState
initNewBoardState = do
    let figures = AllFigures (initFigures Yellow) (initFigures Green) (initFigures Blue) (initFigures Red)
    BoardState{ figures = figures , turn = Yellow } -- TODO: Gelb beginnt immer?

initFigures :: Color -> [Figure]
initFigures color = [Figure x color (Field Start x) | x <- [0..3]]

getFiguresOfColor :: AllFigures -> Color -> [Figure]
getFiguresOfColor allFigures color
  | color == Yellow = yellowFigures allFigures
  | color == Green  = greenFigures allFigures
  | color == Blue   = blueFigures allFigures
  | otherwise       = redFigures allFigures

getBoardOffsetOfColor :: Color -> Int
getBoardOffsetOfColor color = 
  case color of
    Yellow  -> 0
    Green   -> 14
    Blue    -> 28
    _       -> 42

getHomeOffsetOfColor :: Color -> Int
getHomeOffsetOfColor color =
  case color of
    Yellow  -> 52
    Green   -> 10
    Blue    -> 24
    _       -> 38

intToField :: Int -> Field
intToField x
  | x == 0          = Field First x
  | x == 14         = Field First x
  | x == 28         = Field First x
  | x == 42         = Field First x
  | elem x [10..13] = Field Home x
  | elem x [24..27] = Field Home x
  | elem x [38..41] = Field Home x
  | elem x [52..55] = Field Home x
  | otherwise       = Field Standard x

