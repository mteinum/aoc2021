module Day2 where

test = [
 "forward 5",
 "down 5",
 "forward 8",
 "up 3",
 "down 8",
 "forward 2"
 ]

move1 :: [String] -> (Int,Int) -> (Int,Int)
move1 ["forward",n] (pos,depth) = (pos + read n, depth)
move1 ["up",n]      (pos,depth) = (pos, depth - read n)
move1 ["down",n]    (pos,depth) = (pos, depth + read n)

solve1 :: (Int, Int) -> [[String]] -> Int
solve1 (a,b) [] = a * b
solve1 state (x:xs) = solve1 (move1 x state) xs

part1 = do
  content <- readFile("input_day2.txt")
  let result = solve1 (0,0) . map words . lines $ content
  return result

move' :: [String] -> (Int,Int,Int) -> (Int,Int,Int)
move' ["down",n]    (pos, depth, aim) = (pos, depth, aim + read n)
move' ["up",n]      (pos, depth, aim) = (pos, depth, aim - read n)
move' ["forward",n] (pos, depth, aim) = (pos + read n, depth + aim * read n, aim)

solve2 :: (Int, Int, Int) -> [[String]] -> Int
solve2 (pos, depth, aim) [] = pos * depth
solve2 state (x:xs) = solve2 (move' x state) xs

part2 = do
  content <- readFile("input_day2.txt")
  let result = solve2 (0,0,0) . map words . lines $ content
  return result