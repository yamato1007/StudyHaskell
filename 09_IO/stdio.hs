func :: String -> String
func str = unlines $ map (filter (\c -> 'a' < c && c < 'z')) $ lines str


main = do
  str <- getContents
  putStr $ func str
