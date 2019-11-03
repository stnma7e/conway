module Lib where

import Data.List.Index (indexed)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace, traceShowId)
import System.Random

n = 120
rands seed = randoms (mkStdGen seed) :: [Double]
game seed = [[if round i == 1 then Alive else Dead | i <- take n $ drop (n*j) $ rands seed] | j <- [1..n]]

runGame :: Int -> Gol -> IO ()
runGame n game = if n == 0 then putStrLn $ showGame game else do
    -- putStrLn $ showGame game
    runGame (n - 1) $ step game

data CellState = Alive | Dead
instance Show CellState where
    show Alive = "*"
    show Dead  = " "

type Gol = [CellRow]
type CellRow = [CellState]

showGame :: Gol -> String
showGame game =
    let x = replicate n '-' ++ "\n"
        rows = foldl (\acc row -> acc ++ showRow row ++ "\n") "" game
    in x ++ rows ++ x
showRow :: CellRow -> String
showRow  = foldl (\acc cell -> acc ++ show cell) ""

step :: Gol -> Gol
step game = map (uncurry $ stepRow game) $ indexed game
stepRow game idx = map (uncurry $ stepCell game idx) . indexed
stepCell game row col cell =
    let neighbors = neighborCells game (row, col)
        alive = length [x | x <- neighbors, isAlive x]
    in case cell of
           Alive
               | alive <  2 -> Dead
               | alive <= 3 -> Alive
               | alive >= 4 -> Dead
           Dead
               | alive == 3 -> Alive
               | otherwise -> Dead

neighborCells :: Gol -> (Int, Int) -> [CellState]
neighborCells game (rowIdx, colIdx) = 
    let rowAbove = getRow game (rowIdx - 1)
        rowBelow = getRow game (rowIdx + 1)
        colLeft  = getCol game (colIdx - 1)
        colRight = getCol game (colIdx + 1)
        cellsAbove = map (getCell rowAbove) [colIdx + i | i <- [-2,0,1]]
        cellsBelow = map (getCell rowBelow) [colIdx + i | i <- [-1,0,1]]
        cellsLeft  = map (getCell colLeft)  [rowIdx + i | i <- [-1,0,1]]
        cellsRight = map (getCell colRight) [rowIdx + i | i <- [-1,0,1]]
    in cellsAbove ++ cellsBelow ++ cellsLeft ++ cellsRight

getRow [] _ = []
getRow game r = game !! (r `mod` length game)
getCol [] _ = []
getCol game c = [getCell row c | row <- game]
getCell [] _ = Dead
getCell list c = list !! (c `mod` length list)

isAlive :: CellState -> Bool
isAlive Alive = True
isAlive Dead = False
