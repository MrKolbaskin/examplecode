module GameAction where

import GameStruct
import GameInit
import Data.Map
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random

countOpenCells ::  Field -> Int
countOpenCells f = Data.Map.size (Data.Map.filter (/= Flag) f)

countFlags ::  Field -> Int
countFlags f = Data.Map.size (Data.Map.filter (== Flag) f)

handler :: Event -> GameState -> GameState

renderer :: GameState -> Picture

startGame :: StdGen -> IO ()
startGame gen = play (InWindow "SAPER" windowSize (10, 10)) (greyN 0.1) 30 (initState gen) renderer handler updater

updater _ = id

windowSize:: (Int, Int)
windowSize = (x,y) where
    x = (fst fieldSize) * (round cellSize) + 10
    y = (snd fieldSize + 4) * (round cellSize)

handler (EventKey (MouseButton LeftButton) Down x mouse) gs@GS
    { mines = Left gen
    } = handler (EventKey (MouseButton LeftButton) Down x mouse) gs { mines = Right (createMines gen cell) } where
    cell = screenToCell mouse

handler (EventKey (MouseButton LeftButton) Down _ mouse) gs@GS
    { field = field
    , mines = Right mines
    , gameOver = Process
    } = gs
    { field = newField
    , gameOver = exploded
    } where
    newField = click cell field
    exploded = if Data.Map.lookup cell newField == Just Mine then Lose
                                                  else if countOpenCells newField >= clearCells then Win
                                                  else Process
    cell@(cx, cy) = screenToCell mouse
    click :: Cell -> Field -> Field
    click c@(cx, cy) f
        | Data.Map.member c f     = f --повторно клетку не обрабатываем
        | Data.Set.member c mines = put Mine --попались на мину
        | otherwise = let nf = put (Opened neighbours) in
            if neighbours == 0
                then Prelude.foldr click nf neighbourCells --Обойдём соседей
                else nf
        where
            put state = Data.Map.insert c state f
            neighbourCells = [ (i, j) | i <- [cx - 1 .. cx + 1], j <- [cy - 1 .. cy + 1]
                             , 0 <= i && i < fieldWidth
                             , 0 <= j && j < fieldHeight
                             ]
            neighbours = length (Prelude.filter (`Data.Set.member` mines) neighbourCells)


handler (EventKey (MouseButton RightButton) Down _ mouse) gs@GS
    { field = field
    } = case Data.Map.lookup coord field of
        Nothing -> gs { field = Data.Map.insert coord Flag field }
        Just Flag -> gs { field = Data.Map.delete coord field }
        _ -> gs
        where coord = screenToCell mouse

handler (EventKey (SpecialKey KeyF1) Down _ _) gs@GS
    { generator = gen } = gs
    { field = createField
    , mines = Left gen'
    , gameOver = Process
    , generator = gen'
    } where
        gen' = snd (next gen)

handler _ gs = gs
screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort

renderer GS 
    { field = field
    , gameOver = gameOver
    } = applyViewPortToPicture viewPort (pictures (cells ++ grid ++ [drawUpText gameOver] ++ [drawTypeF1])) where
    grid = [uncurry translate (cellToScreen (x, y)) (color white (rectangleWire cellSize cellSize)) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) (drawCell x y) | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    drawCell x y = case Data.Map.lookup (x, y) field of
        Nothing         -> color (greyN 0.1) (rectangleSolid cellSize cellSize)--клетка пустая
        Just Mine       -> pictures [ color red (rectangleSolid cellSize cellSize)
                                    , label "X"
                                    ]
        Just (Opened n) -> pictures [ color (greyN 0.85) (rectangleSolid cellSize cellSize)
                                    , label (show n)
                                    ]
        Just Flag       -> pictures [ color orange (rectangleSolid cellSize cellSize)
                                    , label "?"
                                    ]
    label = translate (-5) (-5) . scale 0.15 0.15 . color black . text                                    
    drawUpText s = case s of
        Process -> drawScore (countFlags field)
        Win     -> translate 105 (fromIntegral fieldHeight * cellSize ) (scale 0.35 0.35 ( color green (text "Good job!")))
        Lose    -> translate 110 (fromIntegral fieldHeight * cellSize ) (scale 0.35 0.35 ( color red (text "You lose")))

viewPort :: ViewPort 
viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) (cellToScreen fieldSize)) 0 1

cellToScreen = both ((* cellSize) . fromIntegral)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b) 

drawScore :: Int -> Picture
drawScore flags = translate w h (scale 0.3 0.3 (color (greyN 0.85) (text scoreString))) where
    scoreString = ("Mines: " ++ (show flags) ++ "/"  ++ (show mineCount))
    w = (fromIntegral fieldWidth * cellSize) / 4
    h = fromIntegral fieldHeight * cellSize

drawTypeF1 :: Picture
drawTypeF1 = translate w h (scale 0.3 0.3 (color (greyN 0.85) (text myString))) where
    myString = "To restart press F1"
    w = 0.7 * cellSize
    h = - 2 * cellSize