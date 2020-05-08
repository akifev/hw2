module Report where

reportCommandOptionsError :: String -> IO ()
reportCommandOptionsError message = do
  putStr "... OOPS! Command options error. "
  putStrLn message

reportIncorrectPath :: String -> IO ()
reportIncorrectPath message = do
  putStr "... OOPS! Incorrect path. "
  putStrLn message
  
reportUnknownCommand :: String -> IO ()
reportUnknownCommand message = do
  putStr "... OOPS! Unknown command. "
  putStrLn message