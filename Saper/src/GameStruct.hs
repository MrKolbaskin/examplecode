module GameStruct where

import Data.Map
import System.Random
import Data.Set

type Field = Map Cell CellState
type Cell = (Int, Int)
type Mines = Set Cell

data CellState = Opened Int --Открыта; параметр — циферка, которая будет отображаться
                | Mine       --Подорвались; без параметров
                | Flag deriving (Eq)      --Поставлен флажок

data GameOver = Process
				| Win 
				| Lose 

data GameState = GS
    { field    :: Field
    , mines    :: Either StdGen Mines
    , gameOver :: GameOver
    , generator :: StdGen
    }


fieldSize :: (Int, Int)
fieldSize@(fieldWidth, fieldHeight) = (15, 15)

mineCount :: Int
mineCount = 10

clearCells :: Int
clearCells = fieldHeight * fieldWidth - mineCount

cellSize :: Float
cellSize = 30
