import Data.Map
import Data.List

main = do 
    input <- readFile "day3_input.txt"
    putStrLn $ show $ length $ nub $ sort $ locations input

data Move = North | South | East | West

type Loc = (Int, Int)
type World = Map Loc Int

updateWorld :: Move -> Loc -> World -> World 
updateWorld m l w = w'
   where l' = newLoc m l
         w' = insertWith (+) l' 1 w

move :: Char -> Move
move x = case x of
           '^' -> North
           'v' -> South
           '>' -> West
           '<' -> East
           _   -> undefined

newLoc :: Move -> Loc -> Loc
newLoc North (x,y) = (x+1,y)
newLoc South (x,y) = (x-1,y)
newLoc West  (x,y) = (x,y-1)
newLoc East  (x,y) = (x,y+1)


locations :: String -> [Loc]
locations xs = scanl (\acc x -> newLoc x acc) (0,0) $ moves xs  

moves :: String -> [Move]
moves xs = Prelude.map move xs

