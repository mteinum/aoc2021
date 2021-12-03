module Aoc.File (readAocFile) where

readAocFile :: String -> IO String
readAocFile path = do
    readFile ("Aoc/" ++ path)