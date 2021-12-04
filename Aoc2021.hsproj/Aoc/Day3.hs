--  https://adventofcode.com/2021/day/3
module Day3 where

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


extract :: Int->[String]->[Char]
extract index = map (\x -> x !! index)

count :: (Int,Int) -> [Char] -> (Int,Int)
count state [] = state
count (zero, one) (x:xs)
  | x == '1' = count (zero, one + 1) xs
  | x == '0' = count (zero + 1, one) xs
count' = count (0,0)

mostCommonBit (zero, one)
  | zero > one = '0'
  | one > zero = '1'

leastCommonBit (zero, one)
  | zero > one = '1'
  | one > zero = '0'

maxIndex = length (test !! 0)

bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

gammaRate' acc index
  | index == maxIndex = bin2dec acc
  | otherwise = gammaRate' (acc ++ [mostCommonBit . count' . extract index $ test]) (index + 1)
gammaRate = gammaRate' [] 0

epsilonRate' acc index
  | index == maxIndex = bin2dec acc
  | otherwise = epsilonRate' (acc ++ [leastCommonBit . count' . extract index $ test]) (index + 1)
epsilonRate = epsilonRate' [] 0

