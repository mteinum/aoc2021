-- AoC 2021
-- Day 2: Dive!
-- https://adventofcode.com/2021/day/2

module Day2 where

import System.Directory
import File

test = [
 "forward 5",
 "down 5",
 "forward 8",
 "up 3",
 "down 8",
 "forward 2"
 ]

parse :: [String] -> (String, Int)
parse [command, x] = (command, read x)

test' = map (parse . words) test

fileContent :: IO [(String, Int)]
fileContent = do
  content <- readAocFile "input_day2.txt"
  return (map (parse . words) . lines $ content)

--
-- Part One
--

move1 :: (String, Int) -> (Int,Int) -> (Int,Int)
move1 ("forward", n) (pos, depth) = (pos + n, depth)
move1 ("up",      n) (pos, depth) = (pos, depth - n)
move1 ("down",    n) (pos, depth) = (pos, depth + n)

solve1 :: (Int, Int) -> [(String, Int)] -> Int
solve1 (a,b) [] = a * b
solve1 state (x:xs) = solve1 (move1 x state) xs

part1 :: IO Int
part1 = do
  solve1 (0,0) <$> fileContent

--
-- Part Two
--

move' :: (String, Int) -> (Int,Int,Int) -> (Int,Int,Int)
move' ("down",    n) (pos, depth, aim) = (pos, depth, aim + n)
move' ("up",      n) (pos, depth, aim) = (pos, depth, aim - n)
move' ("forward", n) (pos, depth, aim) = (pos + n, depth + aim * n, aim)

solve2 :: (Int, Int, Int) -> [(String, Int)] -> Int
solve2 (pos, depth, _) [] = pos * depth
solve2 state (x:xs) = solve2 (move' x state) xs

part2 :: IO Int
part2 = do
  solve2 (0,0,0) <$> fileContent

{-|

>>> part2

-}
