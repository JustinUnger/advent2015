main = do 
  input <- readFile "day1_input.txt"
  let floors = scanl (\acc x -> case x of 
                                  '(' -> acc + 1
                                  ')' -> acc - 1) 0 input
  let (low, hi) = break (== -1) floors
  print $ length low
