--  https://adventofcode.com/2021/day/3
module Aoc.Day3 where

test = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
    ]

countOne acc [] = acc
countOne acc (x:xs) = countOne acc xs

part1 = countOne 0 test

{-|
>>> part1
0
-}
