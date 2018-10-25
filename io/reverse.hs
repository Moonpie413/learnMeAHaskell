main = do
  line <- getLine
  if null line then
    return ()
  else do
    putStrLn $ reverseLine line
    main

reverseLine :: String -> String
reverseLine = unwords . map reverse . words
