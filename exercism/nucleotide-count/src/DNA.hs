{-# LANGUAGE TupleSections #-}

module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map as M (Map, fromListWith)
import Text.Read

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = M.fromListWith (+) <$>
                      mapM (fmap (,1) . readEither . (: [])) xs
