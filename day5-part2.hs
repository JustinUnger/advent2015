import Data.List

main = do
  input <- readFile "day5_input.txt"
  print $ length $ filter nice $ lines input

foo = do
  input <- readFile "day5_input.txt"
  return (lines input)

nice :: String -> Bool
nice xs = check1' xs && check2 xs

check1 :: String -> Bool
check1 xs = isJust $ find (\x -> length x >= 2) $ groupedPairs xs

check1' xs = case (find (\x -> length x >= 2) $ groupedPairs xs) of
               Nothing -> False
               Just ([x,y]:_)  -> if (x /= y) then True else not (isJust $ trip x xs)

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
