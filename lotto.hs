data Cell = Cell (Integer,Integer,Integer) deriving Show
type Row = [Cell]

get_colliding :: [Row] -> ([[[Cell]]],[[[Cell]]])
get_colliding table = (horizontally table, vertically table) where
    horizontally :: [Row] -> [[[Cell]]]
    horizontally matrix = map (process_row []) matrix
    vertically matrix = horizontally $ transpose matrix 

process_row result [] = result
process_row result row = 
    let choice = same (head row) (tail row)
        row_rest = rest (head row) (tail row) in
    if null choice then process_row result row_rest
    else process_row (((head row):choice):result) row_rest 

same (Cell (val,_,_)) row = filter collide row where
    collide (Cell (val1,_,_)) = val == val1
rest (Cell (val,_,_)) row = filter dontCollide row where
    dontCollide (Cell (val1,_,_)) = val /= val1

transpose matrix = map reverse $ foldl
    (flip $ zipWith (:))
    (replicate (length $ head matrix) [])
    matrix

create_matrix [] _ result = result
create_matrix (row:rows) n result = 
    create_matrix rows (n+1) ((reverse(create_row row n 0 [])):result)
create_row [] _ _ result = result
create_row (val:vals) n m result = create_row vals n (m+1) ((Cell (val,n,m)):result)

table = [[1,1],[2,1]]
check = get_colliding (reverse $ create_matrix table 0 [])
