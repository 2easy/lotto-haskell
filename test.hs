create_empty 0 res = res
create_empty n res = create_empty (n-1) ([]:res)

transpose :: [[Integer]] -> [[Integer]] -> [[Integer]]
transpose [] result = result
transpose (row:rows) result = 
    let result1 = throw row result
        transposed = reverse_all $ transpose rows result1 in
        transposed where
            throw :: [Integer] -> [[Integer]] -> [[Integer]]
            throw [] result = []
            throw (x:xs) (y:ys) = let rest = throw xs ys in
                                  (x:y):rest
            reverse_all [] = []
            reverse_all (row:rows) = let row_r = reverse row
                                         rest = reverse_all rows
                                     in row_r:rest


matrix = [[1,2,3],[4,5,6],[7,8,9]]
dupa = transpose matrix (create_empty 3 [])
