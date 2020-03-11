module MyProj
    ( runField
    ) where

import System.Random
import System.IO
import Data.Map
import Data.Set
import System.Random.Shuffle (shuffle')
import Graphics.Gloss.Interface.Pure.Game

type Field = Map Cell CellState
type Cell = (Int, Int)
type Mines = Set Cell

data CellState = Opened Int
                | Mine
                | Flag

data GameState = GS
    { field :: Field
    , mines :: Either StdGen Mines
    }

fieldSize@(row, col) = (15, 15) :: (Int, Int)

range :: (Int, Int)
range = (0, 100)

mineCount :: Int
mineCount = 40

createField :: Field
createField = Data.Map.empty

createMines :: RandomGen g => g -> Cell -> Mines
createMines g fst = Data.Set.fromList $ Data.Set.take mineCount $ shuffle g $
    [(i, j) | i <- [0 .. row - 1]
            , j <- [0 .. col - 1]
            , (i, j) /= fst]

shuffle :: RandomGen gen => gen -> [a] -> [a]
shuffle g l = shuffle' l (row * col - 1) g


startGame :: StdGen -> IO ()
startGame gen = play (InWindow windowSize (240, 160)) (greyN 0.25) 30 (initState gen) renderer handler updater

windowSize = both (* (round cellSize)) fieldSize
cellSize = 24

initState gen = Gs createField (Left gen)

both f (a, b) = (f a, f b)

updater _ = id
handler _ = id

renderer _ = pictures [uncurry translate (cellToScreen (x, y)) $ color white $ rectangleSolid cellSize cellSize
    | x <- [0 .. row - 1], y <- [0 .. col - 1]]

cellToScreen = both ((* cellSize) . fromIntegral)