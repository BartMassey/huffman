module Huffman
where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Heap as Heap

data (Integral a) =>
    HTree a b = HNode { count :: a,
                        left :: HTree a b,
                        right :: HTree a b }
              | HLeaf { count :: a,
                        label :: b }
    deriving Eq

freq :: (Integral a, Ord b) => [b] -> [HTree a b]
freq l = map make_hleaf (Map.toList tab) where
    tab = foldl' accum Map.empty l
    accum m k = Map.alter incr k m
    incr Nothing = Just 1
    incr (Just x) = Just (x + 1)
    make_hleaf (l, n) = HLeaf { count = n, label = l }

instance (Integral a, Eq b) => Ord (HTree a b) where
    t1 `compare` t2 = count t1 `compare` count t2

makeHTree :: (Integral a, Ord b) => [b] -> HTree a b
makeHTree = treeify . from_list . freq where
    from_list :: (Integral a, Ord b) =>
                 [HTree a b] -> Heap.MinHeap (HTree a b)
    from_list = Heap.fromList
    treeify h = treeify1 v' h' where
        (v', h') = Heap.extractHead h
    treeify1 v h
        | Heap.isEmpty h = v
        | otherwise = treeify2 v v' h' where
            (v', h') = Heap.extractHead h
    treeify2 v1 v2 h = treeify h' where
        h' = Heap.insert v h
        v = HNode { count = count v1 + count v2,
                    left = v1,
                    right = v2 }
