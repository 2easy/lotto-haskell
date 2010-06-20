import List
import System (getArgs)
import Control.Monad

type Cell = (Integer,Integer,Integer)
type Generator a = [a]

main = do
    [x] <- getArgs 
    a <- readFile x
    putStrLn a

--decide :: [[Cell]] -> [Cell] -> Generator [Cell]
decide [] black = return black
decide (choice:rest) black = do
    white <- choice
    let black' = (delete white choice) ++ black
        rest' = map (\\ black') rest
    decide rest' black'

check = decide [[(1,0,0),(1,0,2)],[(2,2,0),(2,2,1)],[(2,0,1),(2,2,1)]] []
check1 = decide [[1,2],[3,4]] []
