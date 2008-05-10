--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the end of this file for license terms.

-- |This module provides a simple but efficient
-- implementation of Huffman coding for compression and
-- decompression.  The implementation builds the Huffman
-- tree using the two-queue method, which could be linear
-- time given a sorted list of frequency tables, but
-- currently is heap-based only for correctness.  The tree
-- is currently not canonical, but this is a bug and will be
-- fixed shortly.
--
-- The typical use case is to construct a frequency table
-- with `freq`, then construct the decoding tree from the
-- frequency table with with `makeHTree`, then construct the
-- encoding table from the decoding tree with `makeHTable`.
-- This enables both `encode` and `decode`.
module Data.Huffman (HTree, HTable, Freq(..),
                     freq, makeHTree, makeHTable,
                     encode, decode)
where

import Data.List as List
import Data.Maybe
import Data.Sequence as Q
import qualified Data.Map as Map
import qualified Data.Heap as Heap

-- |The Huffman decoding tree.

data HTree a = HNode (HTree a) (HTree a) -- ^ The left child
                                         -- is taken when False
                                         -- and the right child when True.
             | HLeaf a -- ^ The leaves are the encoded symbols.
               deriving Eq

-- |The Huffman encoding table.  For each encoded symbol,
-- gives a list of Bool representing the prefix encoding
-- of the symbol.
newtype (Ord a) => HTable a = HTable (Map.Map a [Bool])

-- |An entry in the frequency table, consisting of a count
-- and a symbol.
newtype (Integral a) => Freq a b = Freq (a, HTree b)
    deriving Eq

-- |Ordering of frequency table entries is by increasing frequency.
instance (Integral a, Eq b) => Ord (Freq a b) where
    Freq (c1, _) `compare` Freq (c2, _) = c1 `compare` c2

-- |Compile a frequency table.
freq :: (Integral a, Ord b)
     => [b] -- ^List of symbols which will be encoded but
            -- do not occur (i.e. occur with zero frequency)
            -- in the sample.
     -> [b] -- ^Sample list of symbols to determine frequencies.
     -> [Freq a b] -- ^Frequency table.
freq init l = map make_hleaf (Map.toList tab) where
    accum m k = Map.alter incr k m
    incr Nothing = Just 1
    incr (Just x) = Just $! (x + 1)
    make_hleaf (l, n) = Freq (n, HLeaf l)
    make_init l = Map.fromList (map (\v -> (v, 0)) l)
    tab = foldl' accum (make_init init) l

-- |Compile a Huffman decoding tree.
makeHTree :: (Integral a, Ord b)
          => [Freq a b] -- ^Frequency table.
          -> HTree b -- ^Decoding tree.
makeHTree = treeify . from_list where
    from_list :: (Integral a, Ord b)
              => [Freq a b]
              -> (Seq (Freq a b), Heap.MinHeap (Freq a b))
    from_list l = (Q.empty, Heap.fromList l)
    treeify (q, h) | Heap.isEmpty h ||
                     (not (Q.null q) && e < Heap.head h) =
        treeify1 e (es, h) where e :< es = viewl q
    treeify (q, h) =
        treeify1 e (q, es) where (e, es) = Heap.extractHead h
    treeify1 v (q, h) | Q.null q && Heap.isEmpty h =
        t where (Freq (_, t)) = v
    treeify1 v (q, h) | Heap.isEmpty h ||
                        (not (Q.null q) && e < Heap.head h) =
        treeify2 v e (es, h) where e :< es = viewl q
    treeify1 v (q, h) =
        treeify2 v e (q, es) where (e, es) = Heap.extractHead h
    treeify2 (Freq (c1, v1)) (Freq (c2, v2)) (q, h) =
        treeify (q', h) where q' = Freq (c1 + c2, HNode v1 v2) <| q

-- |Compile a Huffman encoding table.
makeHTable :: (Ord a)
           => HTree a -- ^Huffman encoding tree.
           -> HTable a -- ^Huffman encoding table.
makeHTable t = HTable (walk Map.empty [] t) where
    walk h p (HLeaf l) = Map.insert l (List.reverse p) h
    walk h p (HNode l r) = walk (walk h (False : p) l) (True : p) r

-- |Huffman-encode a list of symbols.
encode :: (Ord a)
       => HTable a -- ^Huffman encoding table.
       -> [a] -- ^Symbols to be encoded.
       -> [Bool] -- ^Encoding.
--- encode (HTable h) = concatMap (fromJust . (flip Map.lookup) h)
encode _ [] = []
encode ht@(HTable h) (e : es) = encode' e' where
    e' = fromJust (Map.lookup e h)
    encode' (b : bs) = b : encode' bs
    encode' [] = encode ht es

-- |Huffman-decode a code-string to its symbols.
decode :: HTree a -- ^Decoding tree.
       -> [Bool] -- ^Code-string.
       -> [a] -- ^Decoded symbols.
decode h = decode' h where
    decode' (HLeaf s) [] = [s]
    decode' (HLeaf s) l = s : decode h l
    decode' (HNode left _) (False : l) = decode' left l
    decode' (HNode _ right) (True : l) = decode' right l

--- Redistribution and use in source and binary forms, with or
--- without modification, are permitted provided that the
--- following conditions are met:
---     * Redistributions of source code must retain the above
---       copyright notice, this list of conditions and the following
---       disclaimer.
---     * Redistributions in binary form must reproduce the
---       above copyright notice, this list of conditions and the
---       following disclaimer in the documentation and/or other
---       materials provided with the distribution.
---     * Neither the name of the copyright holder, nor the names
---       of other affiliated organizations, nor the names
---       of other contributors may be used to endorse or promote
---       products derived from this software without specific prior
---       written permission.
--- 
--- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
--- CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
--- INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
--- MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
--- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
--- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
--- NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
--- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
--- OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.