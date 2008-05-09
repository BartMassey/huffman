module Huffman (HTree(..), makeHTree)
where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Heap as Heap

data (Integral a) =>
    HT a b = HN { count :: a,
                  left :: HT a b,
                  right :: HT a b }
           | HL { count :: a,
                  label :: b }
    deriving Eq

instance (Integral a, Show a, Show b) => Show (HT a b) where
    show h = show' 0 h where
        show' i (HL {count = c, label = l}) =
            replicate i ' ' ++ "<#" ++ show c ++ " " ++ show l ++ ">"
        show' i (HN {count = c, left = l, right = r}) =
            replicate i ' ' ++ "<#" ++ show c ++ "\n" ++
            show' (i + 2) l ++ "\n" ++
            show' (i + 2) r ++ ">"

data HTree a = HNode (HTree a) (HTree a)
             | HLeaf a

freq :: (Integral a, Ord b) => [b] -> [HT a b]
freq l = map make_hleaf (Map.toList tab) where
    tab = foldl' accum Map.empty l
    accum m k = Map.alter incr k m
    incr Nothing = Just 1
    incr (Just x) = Just (x + 1)
    make_hleaf (l, n) = HL { count = n, label = l }

instance (Integral a, Eq b) => Ord (HT a b) where
    t1 `compare` t2 = count t1 `compare` count t2

makeHTree :: (Ord b) => [b] -> HTree b
makeHTree = ht_extract . treeify . from_list . freq where
    from_list :: (Integral a, Ord b) =>
                 [HT a b] -> Heap.MinHeap (HT a b)
    from_list = Heap.fromList
    treeify h = treeify1 v' h' where
        (v', h') = Heap.extractHead h
    treeify1 v h
        | Heap.isEmpty h = v
        | otherwise = treeify2 v v' h' where
            (v', h') = Heap.extractHead h
    treeify2 v1 v2 h = treeify h' where
        h' = Heap.insert v h
        v = HN { count = count v1 + count v2,
                 left = v1,
                 right = v2 }
    ht_extract :: (Integral a) => HT a b -> HTree b
    ht_extract (HN {left = l, right = r}) =
        HNode (ht_extract l) (ht_extract r)
    ht_extract (HL {label = l}) =
        HLeaf l

-- encode :: (Ord b) => HTable a b -> [b] -> [Bool]
-- encode _ [] = []
-- encode h (e : es) = treewalk ++ encode h es where

