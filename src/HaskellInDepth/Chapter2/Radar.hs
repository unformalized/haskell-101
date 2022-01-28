module HaskellInDepth.Chapter2.Radar where

data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show)

data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, Show)

rotate :: Turn -> Direction -> Direction
rotate = undefined

orient :: Direction -> Direction -> Turn
orient = undefined

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = undefined

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = undefined

orientMany :: [Direction] -> [Turn]
orientMany = undefined

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile = undefined

orientFromFile :: FilePath -> IO ()
orientFromFile = undefined
