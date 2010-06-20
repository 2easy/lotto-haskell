import List
import System (getArgs)
import Control.Monad

type Cell = (Integer,Integer,Integer)
type Generator a = [a]

main = do
    [x] <- getArgs 
    a <- readFile x
    putStrLn a

decide :: [[Cell]] -> [Cell] -> [Cell] -> Generator [Cell]
decide [] _ black = return black
decide (choice:rest) white black = do
    whiteCell <- choice
    let newBlack = delete whiteCell choice
    guard $ not (neighbour newBlack black)
    let black' = newBlack ++ black
        white' = white \\ newBlack
    guard $ bfs ([head white']) (tail white')
    let rest' = map (\\ black') rest
    decide rest' black' white'

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
----------------------------BFS-------------------------------
bfs :: [Cell] -> [Cell] -> Bool
bfs _  []    = True
bfs [] (y:_) = False
bfs (current:grey) white = do
    let newGrey = whiteNeighbours current white []
        white' = white \\ newGrey
        grey' = grey ++ newGrey
    bfs grey' white'

whiteNeighbours :: Cell -> [Cell] -> [Cell] -> [Cell]
whiteNeighbours _ [] result = result
whiteNeighbours current@(_,n,m) ((y@(_,n',m')):ys) result = 
    if ((n == n'-1 || n == n'+1) && m == m') ||
       ((m == m'-1 || m == m'+1) && n == n')
    then whiteNeighbours current ys (y:result)
    else whiteNeighbours current ys result
