module GameInit where

import System.Random.Shuffle (shuffle')
import Data.Map
import Data.Set
import System.Random
import GameStruct

createField :: Field
createField = Data.Map.empty

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g l = shuffle' l (fieldWidth * fieldHeight - 1) g

initState :: StdGen -> GameState
initState gen = GS createField (Left gen) Process gen

createMines :: RandomGen g => g -> Cell -> Mines
createMines g fst = Data.Set.fromList $ Prelude.take mineCount (shuffle g (
    [(i, j) | i <- [0 .. fieldWidth - 1]
            , j <- [0 .. fieldHeight - 1]
            , (i, j) /= fst]))
