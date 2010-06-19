module Lotto (Cell, Row, Generator, BlackCoords, Task(..)) where

import Monad
import List ((\\), delete)

type Generator a = [a]

type Cell = (Int,Int,Int)
type BlackCoords = (Int, Int)
type Row = [Cell]

data Task = Task {
        size  :: Int,
        board :: [[Int]]
     }
--Returns colliding values on the given board
getColliding :: [Row] -> [[Cell]]
getColliding table = (horizontally table) ++ (vertically table) where
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
check = 
    getColliding (reverse $ createMatrix table 0 [])
--------------------------------------------------------------------

--------------------------------------------------------------------
solve :: Task -> Generator BlackCoords
solve task = do
    [(1,2)]
