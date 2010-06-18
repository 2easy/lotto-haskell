data Cell a = Cell (Integer,Integer,Integer) deriving Show

same val row = filter collide row where
    collide val1 = val == val1
rest val row = filter (dontCollide) row where
    dontCollide val1 = val /= val1

create_matrix [] _ result = result
create_matrix (row:rows) n result = 
    create_matrix rows (n+1) ((reverse(create_row row n 0 [])):result)
create_row [] _ _ result = result
create_row (val:vals) n m result = create_row vals n (m+1) ((Cell (val,n,m)):result)

table = [[1,2,1],[4,3,2],[2,2,3]]
test = reverse $ create_matrix table 0 []
