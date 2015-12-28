import Data.List
import Data.Maybe

main = do 
   input <- readFile "day5_input.txt"
   let ls = lines input
   print $ length $ filter nice ls 

nice :: String -> Bool
nice xs = check1 xs && check2 xs && check3 xs

check2 :: String -> Bool
check2 xs = case dups of 
            Nothing -> False
            _       -> True 
   where dups = find (\x -> length x >= 2) (group xs)

check3 :: String -> Bool
check3 xs = not $ ab || cd || pq || xy
  where ab = isInfixOf "ab" xs
        cd = isInfixOf "cd" xs
        pq = isInfixOf "pq" xs
        xy = isInfixOf "xy" xs

test1 = "qjhvhtzxzqqjkmpb"
test2 = "xxyxx"
test3 = "uurcxstgmygtbstg"
test4 = "ieodomkazucvgmuy"

test1Pass = nice test1 == True
test2Pass = nice test2 == True
test3Pass = nice test3 == False
test4Pass = nice test4 == False

pairs :: [a] -> [[a]]
pairs [] = []
pairs [_] = []
pairs (x:y:ys) = [x,y] : pairs (y:ys) 

answer xs = find (\x -> length x == 2) (mySort xs) 
mySort = group . sort . pairs

check1 :: String -> Bool
check1 xs = isJust $ answer xs
