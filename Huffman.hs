module Huffman
where

import Data.Maybe
import Data.Map as Map

freq :: (Ord a, Integral b) => [a] -> [(a, b)]
freq l = Map.toList tab where
    tab = foldl accum Map.empty l
    accum m k = Map.alter incr k m
    incr Nothing = Just 1
    incr (Just x) = Just (x + 1)
