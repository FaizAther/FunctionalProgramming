


import Text.Read

interactiveDoubling = do
  putStrLn "Choose number:"
  s <- getLine
  let mx = readMaybe s :: Maybe Double
  case mx of
    Just x -> putStrLn ("The double is: " ++ show (2*x))
    Nothing -> do
      putStrLn "wrong..."
      interactiveDoubling

main = interactiveDoubling
