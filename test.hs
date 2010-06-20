import List
import System (getArgs)
import Control.Monad

type Cell = (Integer,Integer,Integer)
type Generator a = [a]

main = do
    [x] <- getArgs 
    a <- readFile x
    putStrLn a

decide :: [[Cell]] -> [Cell] -> Generator [Cell]
decide [] black = return black
decide (choice:rest) black = do
    white <- choice
    let newBlack = delete white choice
    guard $ not (neighbour newBlack black)
    let black' = newBlack ++ black
        rest' = map (\\ black') rest
    decide rest' black'

neighbour :: [Cell] -> [Cell] -> Bool 
neighbour [] _ = False
neighbour (x:xs) black = if isNei x black then True else neighbour xs black
isNei _ [] = False
isNei x@(_,n,m) ((_,n',m'):ys) =
    if ((n == n'-1 || n == n'+1) && m == m') ||
       ((m == m'-1 || m == m'+1) && n == n')
    then True
    else isNei x ys

check = decide [[(1,0,0),(1,0,2)],[(2,2,0),(2,2,1)],[(2,0,1),(2,2,1)]] []
