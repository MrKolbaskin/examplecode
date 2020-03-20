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
