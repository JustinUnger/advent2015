import Data.List

main = do 
   input <- readFile "day5_input.txt"
   let ls = lines input
   print $ length $ filter nice ls 

nice :: String -> Bool
nice xs = check1 xs && check2 xs && check3 xs

check1 :: String -> Bool
check1 xs = vowels >= 3
  where vowels = length $ xs `intersect` "aeiou"  

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

test1 = "ugknbfddgicrmopn"
test2 = "aaa"
test3 = "jchzalrnumimnmhp"
test4 = "haegwjzuvuyypxyu"
test5 = "dvszwmarrgswjxmb"

test1Pass = nice test1 == True
test2Pass = nice test2 == True
test3Pass = nice test3 == False
test4Pass = nice test4 == False
test5Pass = nice test5 == False
