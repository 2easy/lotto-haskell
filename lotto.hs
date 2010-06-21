-- Solution to assignment no. 3 'Lotto'
-- ghci
-- Usage:
--       ./lotto data_file
-- data_file: 
--       BoardSize. :: Int
--       Board.     :: [[Int]]
-- where
--       BoardSize = 1 .. 16
--       Board = [[val11, ..., val1N],
--                [val12, ..., val2N],
--                ...
--                [valN1, ..., valNN]] 
--       where
--           valNM = 1 .. BoardSize
----------------------------------------------------
module Main where

import Char
import Control.Monad
import List
import System( getArgs )

type Generator a = [a]
type Cell = (Integer,Integer,Integer)
type Row = [Cell]
type BlackCoords = (Integer, Integer)
(?==?) :: Cell -> Cell -> Bool
(?==?) (a,_,_) (b,_,_) = a == b
(?>?) :: Cell -> Cell -> Ordering
(?>?) (a,_,_) (b,_,_) = if a > b then GT else LT
-- Returns colliding values on the given board
getColliding :: [Row] -> [[Cell]]
getColliding board = (horizontally board) ++ (vertically board) where
    horizontally matrix = foldl (++) [] (map processRow matrix)
    vertically matrix = horizontally $ transpose matrix 
-- Returns colliding values in the given row
processRow :: Row -> [[Cell]]
processRow row = filter
                     (\list -> length list > 1) 
                     (groupBy (?==?) (sortBy (?>?) row))
-- Creates matrix of cells from given table of values
createMatrix [] _ result = result
createMatrix (row:rows) n result = 
    createMatrix rows (n+1) ((reverse(createRow row n 0 [])):result)
createRow [] _ _ result = result
createRow (val:vals) n m result = createRow vals n (m+1) (((val,n,m)):result)
-- Parsing input --
parse :: String -> IO (Integer,[[Integer]])
parse content = 
    let [sizeStr, boardStr] = 
            words $ map 
                        (\c -> if c == '.' then ' ' else c) 
                        (filter (not.isSpace) content) in
    return (read sizeStr, read boardStr::[[Integer]])
--------------------------------------------------------------------
decide :: [[Cell]] -> [Cell] -> [Cell] -> Generator [Cell]
decide [] _ black = return black
decide ([]:rest) white black = decide rest white black
decide (choice:rest) white black = do
    whiteCell <- choice
    let newBlack = delete whiteCell choice
        black' = newBlack ++ black
        white' = white \\ newBlack
        rest'  = map (\\ black') rest
    guard (not (neighbour newBlack black) && 
           not (neighbour newBlack newBlack)
          ) 
    guard (bfs ([head white']) (tail white'))
    decide rest' white' black'

neighbour :: [Cell] -> [Cell] -> Bool 
neighbour candidates black = any ((flip isNei) black) candidates 
isNei (_,n,m) ys = any neiCoords ys where
    neiCoords (_,n',m') = ((n == n'-1 || n == n'+1) && m == m') ||
                          ((m == m'-1 || m == m'+1) && n == n') 
----------------------------BFS-------------------------------
bfs :: [Cell] -> [Cell] -> Bool
bfs _  []    = True
bfs [] (y:_) = False
bfs (black:grey) white = do
    let newGrey = whiteNeighbours black white []
        white' = white \\ newGrey
        grey' = grey ++ newGrey
    bfs grey' white'

whiteNeighbours :: Cell -> [Cell] -> [Cell] -> [Cell]
whiteNeighbours _ [] result = result
whiteNeighbours black@(_,n,m) ((y@(_,n',m')):ys) result = 
    if ((n == n'-1 || n == n'+1) && m == m') ||
       ((m == m'-1 || m == m'+1) && n == n')
    then whiteNeighbours black ys (y:result)
    else whiteNeighbours black ys result
------------------------------------------------------------------
solve :: [[Integer]] -> Generator [Cell]
solve board = do
    let matrix     = reverse $ createMatrix board 0 []
        options    = getColliding matrix
        allCells   = foldl (++) [] matrix
    blackCells <- decide options allCells []
    [blackCells]
------------------------------------------------------------------
subsets :: [Cell] -> Generator [Cell]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ (map (addNotNei x) (subsets xs)) where
    addNotNei x subset = 
        if isNei x subset then []
        else x:subset            
-------------------------Print-------------------------------------
ripAll :: Generator [Cell] -> Generator [BlackCoords]

ripAll = map ripVal

ripVal :: [Cell] -> [BlackCoords]
ripVal cells = map (\(_,n,m) -> (n,m)) cells
makePrintable [] result = result
makePrintable (row:rows) result =
    makePrintable rows ((map (\(val,a,b) -> 
                                 if val > 10 then (show val,a,b) 
                                 else (" " ++ (show val),a,b))
                             row) : result)
printAll :: [[(String,Integer,Integer)]] -> Generator [BlackCoords] -> IO ()
printAll matrix [] = return ()
printAll matrix (x:xs) =
    (myPrint matrix x) >>
    putStrLn "" >>
    printAll matrix xs
myPrint :: [[(String,Integer,Integer)]] -> [BlackCoords] -> IO ()
myPrint [] _ = return ()
myPrint (row:rows) black = do
    printRow row black
    myPrint rows black
printRow [] _ = putStrLn ""
printRow ((sVal,n,m):cells) black =
    (if ((n,m) `elem` black) then (putStr " #") 
    else (putStr sVal)) >>
    printRow cells black
---------------Main--------------------
main = do
    [file_name]   <- getArgs 
    content       <- readFile file_name
    (size,board)  <- parse content
    solution      <- return $ ripAll (solve board)
    printAll (makePrintable (createMatrix board 0 []) []) solution
