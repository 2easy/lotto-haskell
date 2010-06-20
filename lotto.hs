module Main where

import Char
import Control.Monad
import List --((\\), delete, transpose)
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
getColliding :: [Row] -> IO [[Cell]]
getColliding board = return $ (horizontally board) ++ (vertically board) where
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
writeAll :: [String] -> IO ()
writeAll [] = return ()
writeAll (x:xs) = do
    putStrLn x 
    putStr "next: "
    writeAll xs
--------------------------------------------------------------------
main = do
    [file_name]  <- getArgs 
    content      <- readFile file_name
    (size,board) <- parse content
    options      <- getColliding (reverse $ createMatrix board 0 [])
--    black        <- decide options []
    putStrLn $ show options
    putStrLn $ show size
    putStrLn $ show board
