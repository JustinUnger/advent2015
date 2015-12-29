{- 
I found this one rather difficult. Trying to check for overlap on check1 took a long time. Most people seemed to solve this with regex. 
-}


import Data.List
import Data.Ord

main = do
  input <- readFile "day5_input.txt"
  print $ length $ filter nice $ lines input

foo = do
  input <- readFile "day5_input.txt"
  return (lines input)

nice :: String -> Bool
nice xs = check1 xs && check2 xs

check1' :: String -> Bool
check1' xs = isJust $ find (\x -> length x >= 2) $ groupedPairs xs

check1 xs = check1' xs && not (overlap xs)

overlap xs = case find (\x -> length x >= 2) (proximity xs) of
                Nothing -> True
                Just xs -> if (fst $ last xs) - (fst $ head xs) < 2 then True else False 

proximity xs = groupBy (\x y -> snd x == snd y) $ sortBy (comparing snd) $ zip [0..] $ pairs xs

trip x xs = find (\(a,b,c) -> a == b && b == c && a == x) (triples xs)

groupedPairs xs = group $ sort $ pairs xs

check2 :: String -> Bool
check2 xs = isJust $ find (\(x,y,z) -> x == z) $ triples xs

pairs :: [a] -> [[a]]
pairs [] = []
pairs [x] = []
pairs (x:y:ys) = [x,y] : pairs (y:ys)

pairs' :: [a] -> [[a]]
pairs' (x:y:[]) = [[x,y]]
pairs' (x:y:ys) = [x,y] : pairs ys
pairs' _        = []

triples :: String -> [(Char,Char,Char)]
triples (x:y:z:zs) = (x,y,z) : triples (y:z:zs)
triples _          = []

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust       _ = True

test1 = "qjhvhtzxzqqjkmpb"
test2 = "xxyxx"
test3 = "uurcxstgmygtbstg"
test4 = "ieodomkazucvgmuy"
test5 = "aaa"
test6 = "xilodxfuxphuiiii"
test7 = "qpnxkuldeiituggg"

test1Pass = nice test1 == True
test2Pass = nice test2 == True
test3Pass = nice test3 == False
test4Pass = nice test4 == False
test5Pass = nice test5 == False
test6Pass = nice test6 == True
test7Pass = nice test7 == False

testPass = and [test1Pass, test2Pass, test3Pass, test4Pass, test5Pass, test6Pass, test7Pass]
