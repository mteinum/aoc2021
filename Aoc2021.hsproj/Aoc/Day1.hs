import Data.List.Split

depths = [199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263]


part1 :: Int -> Int -> [Int] -> Int
part1 state prev [] = state
part1 state prev (x:xs)
  | x > prev  = part1 (state + 1) x xs
  | otherwise = part1 state x xs

part1_ :: [Int] -> Int
part1_ (x:xs) = part1 0 x (x:xs)

readFileIntLines :: IO [Int]
readFileIntLines = do
  content <- readFile("input_day1.txt")
  return (map read . lines $ content)

solve1 :: IO Int
solve1 = do
  content <- readFileIntLines
  let answer = part1_ content
  return (answer)

