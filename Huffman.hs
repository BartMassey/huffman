module Huffman (HTree, makeHTree, HTable, makeHTable, encode, decode)
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

data HTree a = HNode (HTree a) (HTree a)
             | HLeaf a

newtype (Ord a) => HTable a = HTable (Map.Map a [Bool])

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

makeHTable :: (Ord a) => HTree a -> HTable a
makeHTable t = HTable (walk Map.empty [] t) where
    walk h p (HLeaf l) = Map.insert l (reverse p) h
    walk h p (HNode l r) = walk (walk h (False : p) l) (True : p) r

encode :: (Ord a) => HTable a -> [a] -> [Bool]
encode (HTable h) = concatMap (fromJust . (flip Map.lookup) h)

decode :: HTree a -> [Bool] -> [a]
decode h = decode' h where
    decode' (HLeaf s) l = s : decode h l
    decode' _ [] = []
    decode' (HNode left _) (False : l) = decode' left l
    decode' (HNode _ right) (True : l) = decode' right l
                                             
