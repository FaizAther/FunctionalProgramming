import Data.Char

greet :: IO ()
greet = do
  putStrLn "What is your name?"
  name <- getLine
  let uname = map toUpper name
  putStrLn ("Hello " ++ name)


main :: IO ()
main = do
  i <- getLine
  if i /= "quit" then do
    greet
    main
  else
    return ()
