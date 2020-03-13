module GameAction where

import GameStruct
import GameInit
import Data.Map
import Data.Set
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

countOpenCells ::  Field -> Int
countOpenCells f = Data.Map.size (Data.Map.filter (/= Flag) f)

handler :: Event -> GameState -> GameState

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
            neighbours = length $ Prelude.filter (`Data.Set.member` mines) neighbourCells


handler (EventKey (MouseButton RightButton) Down _ mouse) gs@GS
    { field = field
    } = case Data.Map.lookup coord field of
        Nothing -> gs { field = Data.Map.insert coord Flag field }
        Just Flag -> gs { field = Data.Map.delete coord field }
        _ -> gs
        where coord = screenToCell mouse
handler _ gs = gs
screenToCell = both (round . (/ cellSize)) . invertViewPort viewPort

renderer GS 
    { field = field
    , gameOver = Process
    } = applyViewPortToPicture viewPort $ pictures $ cells ++ grid where
    grid = [uncurry translate (cellToScreen (x, y)) $ color black $ rectangleWire cellSize cellSize | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    cells = [uncurry translate (cellToScreen (x, y)) $ drawCell x y | x <- [0 .. fieldWidth - 1], y <- [0 .. fieldHeight - 1]]
    drawCell x y = case Data.Map.lookup (x, y) field of
        Nothing         -> color white $ rectangleSolid cellSize cellSize --клетка пустая
        Just Mine       -> pictures [ color red $ rectangleSolid cellSize cellSize
                                    , label "@"
                                    ]
        Just (Opened n) -> pictures [ color green $ rectangleSolid cellSize cellSize
                                    , label (show n)
                                    ]
        Just Flag       -> pictures [ color yellow $ rectangleSolid cellSize cellSize
                                    , label "?"
                                    ]
    label = translate (-5) (-5) . scale 0.15 0.15 . color black . text

renderer GS {gameOver = Lose} = applyViewPortToPicture viewPort (label "You Lose") where
    label = translate (20) (120) . scale 0.5 0.5 . color red . text

renderer GS {gameOver = Win} = applyViewPortToPicture viewPort (label "Good job!") where
    label = translate (20) (120) . scale 0.5 0.5 . color red . text

viewPort = ViewPort (both (negate . (/ 2) . (subtract cellSize)) $ cellToScreen fieldSize) 0 1

cellToScreen = both ((* cellSize) . fromIntegral)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b) 
