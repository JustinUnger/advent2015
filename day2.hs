main = do
    input <- readFile "day2_input.txt"
    let boxes = map conv' $ map conv $ map (wordsWhen (== 'x')) $ lines input
    putStrLn $ show $ foldl (\acc x -> acc + paper x) 0 boxes
    putStrLn $ show $ foldl (\acc x -> acc + ribbon x) 0 boxes

data Box = Box Int Int Int deriving Show

conv' :: [Int] -> Box
conv' (l:w:h:[]) = Box l w h

conv :: [String] -> [Int]
conv xs = map read $ xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of 
                    "" -> []
                    s' -> w : wordsWhen p s''
                          where (w, s'') = break p s'

surface :: Box -> Int
surface (Box l w h) = 2*l*w + 2*w*h + 2*h*l

areaSmallestSide :: Box -> Int
areaSmallestSide (Box l w h) = (l*w) `min` (w*h) `min` (h*l)

perimeterSmallestSide :: Box -> Int
perimeterSmallestSide (Box l w h) = (2*l+2*w) `min` (2*w+2*h) `min` (2*h+2*l)

volume :: Box -> Int
volume (Box l w h) = l*w*h

ribbon :: Box -> Int
ribbon b = perimeterSmallestSide b + volume b

paper :: Box -> Int
paper b = surface b + areaSmallestSide b
