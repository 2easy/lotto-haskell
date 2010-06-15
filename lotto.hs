data Cell = Cell (Integer,Integer,Integer)
type Row = [Cell]

get_colliding :: [Row] -> ([[Cell]],[[Cell]])
get_colliding table = (horizontally table [], vertically table) where
    horizontally :: [Row] -> [Row] -> [[Cell]]
    horizontally [] res = res
    horizontally (row:rows) res = 
        let choises = aux row [] in
        horizontally rows (choises:res) where
            aux :: Row -> [Cell] -> [Cell]
            aux [] res = res
            aux (x:xs) res = 
                let (same,rest) = sameRest x xs ([],[]) in
                aux rest (same:res) where
                    sameRest _ [] res = res
                    sameRest Cell (a,_,_) ((y@(b,_,_)):tail) (same,rest)
                        | a == b    = sameRest x tail (y:same,rest)
                        | otherwise = sameRest x tail (same,y:rest) 
    vertically matrix@(row:rows) =
        let empty = take (length row) (repeat [])
            transposed = transpose matrix empty in
        horizontally transposed [] where
            transpose [] result = result
            transpose (row:rows) result = 
                let result1 = throw row result
                    transposed = map reverse (transpose rows result1) in
                transposed where
                    throw [] result = []
                    throw (x:xs) (y:ys) = let rest = throw xs ys in
                                          (x:y):rest

matrix = [[1,2,1],[4,3,2],[2,2,3]]

check = get_colliding matrix
