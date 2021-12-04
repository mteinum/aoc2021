module File (readAocFile) where

readAocFile :: String -> IO String
readAocFile path = do
    readFile (path)