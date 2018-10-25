
main = do
  contents <- getContents
  putStrLn $ shortLinesOnly contents

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter ((<10) . length) . lines
