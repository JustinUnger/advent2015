main = do 
  input <- readFile "day1_input.txt"
  let floor = foldr (\x acc -> case x of 
                                  '(' -> acc + 1
                                  ')' -> acc - 1) 0 input
  print floor
