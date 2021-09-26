module Acronym (abbreviate) where

import qualified Data.Text as T
import qualified Data.Char as C

abbreviate :: String -> String
abbreviate = go ""
  where
    go acc []     = acc
    go acc (x:xs)
      | C.isUpper x = go (acc ++ [x]) xs
      | C.isSpace x = go (go' acc xs) xs
      | otherwise = go acc xs
    go' :: String -> String -> String
    go' acc [] = acc
    go' acc (y:ys) = acc ++ [y]

