module Main where

import System.Random
import Graphics.Gloss
import GameInit
import GameAction
import GameStruct

main :: IO ()
main = do
    gen <- getStdGen
    startGame gen

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "Hsweeper" windowSize (240, 160)) white 30 (initState gen) renderer handler updater

updater _ = id

windowSize = both (* (round cellSize)) fieldSize
