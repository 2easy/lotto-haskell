module Main where

import Char
import Control.Monad
import List ((\\), delete)
import System( getArgs )

type Generator a = [a]
type Cell = (Integer,Integer,Integer)
type BlackCoords = (Integer, Integer)
type Row = [Cell]

--Returns colliding values on the given board
getColliding :: [Row] -> IO [[Cell]]
getColliding table = return $ (horizontally table) ++ (vertically table) where
    horizontally matrix = filter (not.null) $ map (processRow []) matrix
    vertically matrix = horizontally $ transpose matrix 

transpose matrix = map reverse $ foldl
    (flip $ zipWith (:))
    (replicate (length $ head matrix) [])
    matrix
--Returns colliding values in the given row
processRow result [] = result
processRow result row = 
    let options = same (head row) (tail row)
        row_rest = rest (head row) (tail row) in
    if null options then processRow result row_rest
    else processRow (((head row):options) ++ result) row_rest 

same ((val,_,_)) row = [ cell | cell@(val1,_,_) <- row, val1 == val ]
rest ((val,_,_)) row = [ cell | cell@(val1,_,_) <- row, val1 /= val ]
--Creates matrix of cells from given table of values
createMatrix [] _ result = result
createMatrix (row:rows) n result = 
    createMatrix rows (n+1) ((reverse(createRow row n 0 [])):result)
createRow [] _ _ result = result
createRow (val:vals) n m result = createRow vals n (m+1) (((val,n,m)):result)

table = [[1,1],[2,1]]
table1 = [[1,2,1],[4,3,2],[2,2,3]]
check = 
    getColliding (reverse $ createMatrix table 0 [])
--------------------------------------------------------------------
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
solve :: Task -> Generator BlackCoords
solve task = do
    [(1,2)]
main = do
    [file_name] <- getArgs 
    content <- readFile file_name
    (size,board) <- parse content
    cells <- getColliding (reverse $ createMatrix board 0 [])
    putStrLn $ show cells
    putStrLn $ show size
    putStrLn $ show board
