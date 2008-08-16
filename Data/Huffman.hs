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
module Data.Huffman (HTree, HTable, Freq,
                     freq, fromFreq, toFreq, recount,
                     makeHTree, makeHTable,
                     encode, decode,
                     reconstructHTree)
where

import Data.List
import Data.Maybe
import Data.Sequence as Q (ViewL(..), (|>), viewl, empty, null)
import Data.Map as M (Map, empty, toList, fromList, insert, lookup, alter)
import Data.Ord
import Data.Function

-- |The Huffman decoding tree.

data HTree a = HNode { depth :: Int,  -- ^ Length of longest path
                       left :: HTree a, -- ^ Child for False == 0
                       right :: HTree a } -- ^ Child for True == 1
             | HLeaf a -- ^ The leaves are the encoded symbols.
             deriving (Eq, Show)

-- |The Huffman encoding table.  For each encoded symbol,
-- gives a list of Bool representing the prefix encoding
-- of the symbol.
newtype (Ord a) => HTable a = HTable (Map a [Bool]) deriving Eq

instance (Show a, Ord a) => Show (HTable a) where
    show (HTable m) = show (toList m)

-- |An entry in the frequency table, consisting of a count
-- and a symbol.
newtype (Integral a) => Freq a b = Freq (a, HTree b)
    deriving Eq

instance (Integral a, Show b) => Show (Freq a b) where
    show (Freq (n, HLeaf s)) = show (n, s)

-- |Ordering of Huffman trees is lexicographic by height,
-- then by leaf symbol ordering.
instance (Ord a) => Ord (HTree a) where
    HLeaf _ `compare` HNode {} = LT
    HNode {} `compare` HLeaf _ = GT
    HLeaf l1 `compare` HLeaf l2 = l1 `compare` l2
    n1 `compare` n2 = (depth n1, left n1, right n1) `compare`
                      (depth n2, left n2, right n2)

-- |Ordering of frequency table entries is by increasing frequency,
-- then by lexicographic ordering of trees.
instance (Integral a, Ord b) => Ord (Freq a b) where
    Freq f1 `compare` Freq f2 = f1 `compare` f2

-- |Compile a frequency table.
freq :: (Integral a, Ord b)
     => [b] -- ^List of symbols which will be encoded but
            -- do not occur (i.e. occur with zero frequency)
            -- in the sample.
     -> [b] -- ^Sample list of symbols to determine frequencies.
     -> [Freq a b] -- ^Frequency table.
freq init l = map make_hleaf (toList tab) where
    accum m k = alter incr k m
    incr Nothing = Just 1
    incr (Just x) = Just $! (x + 1)
    make_hleaf (l, n) = Freq (n, HLeaf l)
    make_init l = fromList (map (\v -> (v, 0)) l)
    tab = foldl' accum (make_init init) l

-- |Convert a frequency table entry to a tuple.  The entry
-- must be for a symbol, not a tree.
fromFreq :: (Integral a) => Freq a b -> (a, b)
fromFreq (Freq (c, HLeaf s)) = (c, s)
                             
-- |Convert a tuple to a frequency table entry.
toFreq :: (Integral a) => (a, b) -> Freq a b
toFreq (c, s) = Freq (c, HLeaf s)

-- |Normalize a frequency table so that the largest count is
-- scaled down to the given value if necessary.
recount :: (Integral a, Integral b, Ord c)
        => a  -- ^Max normalized value.
        -> [Freq b c]  -- ^Frequency table to normalize.
        -> [Freq a c]  -- ^Normalized frequency table.
recount m' f = result where
    m = maximum (map count f)
    count (Freq (c, _)) = c
    coerce (Freq (c, t)) = Freq (fromIntegral c, t)
    rescale (Freq (c, t)) =
        Freq (round (fromIntegral m' * fromIntegral c / fromIntegral m), t)
    result = if m <= fromIntegral m' then map coerce f else map rescale f

tree_depth (HLeaf _) = 0
tree_depth (HNode {depth = d}) = d

-- |Compile a Huffman decoding tree.
makeHTree :: (Integral a, Ord b)
          => [Freq a b] -- ^Frequency table.  Must be in
                        -- non-descending order by frequency; otherwise
                        -- makeHTree will fail.
          -> HTree b -- ^Decoding tree.
makeHTree l = treeify (Q.empty, l) where
    extract_min (q, e1 : es) | Q.null q || e1 < q1 = (e1, (q, es)) where
        q1 :< _ = viewl q
    extract_min (q, h) = (q1, (qs, h)) where
        q1 :< qs = viewl q
    treeify (q, []) | Q.null qs = t where
        Freq (_, t) :< qs = viewl q
    treeify (_, (e1 : e2 : _)) | e2 < e1 =
        error "frequency table out of order"
    treeify (q, h) = treeify (q3, h2) where
        (Freq (c1, v1), qh1) = extract_min (q, h)
        (Freq (c2, v2), (q2, h2)) = extract_min qh1
        q3 = q2 |> Freq (c1 + c2, n3)
        n3 = HNode { depth = 1 + (max `on` tree_depth) v1 v2,
                     left = v1 `min` v2,
                     right = v1 `max` v2 }

-- |Compile a Huffman encoding table.
makeHTable :: (Ord a)
           => HTree a -- ^Huffman encoding tree.
           -> HTable a -- ^Huffman encoding table.
makeHTable t = HTable (walk M.empty [] t) where
    walk h p (HLeaf l) = M.insert l (reverse p) h
    walk h p (HNode {left = l, right = r}) =
        walk (walk h (False : p) l) (True : p) r

-- |Huffman-encode a list of symbols.
encode :: (Ord a)
       => HTable a -- ^Huffman encoding table.
       -> [a] -- ^Symbols to be encoded.
       -> [Bool] -- ^Encoding.
encode _ [] = []
encode ht@(HTable h) (e : es) = encode' e' where
    e' = fromJust (M.lookup e h)
    encode' (b : bs) = b : encode' bs
    encode' [] = encode ht es

-- |Huffman-decode a code-string to its symbols.
decode :: (Show a)
       => HTree a -- ^Decoding tree.
       -> [Bool] -- ^Code-string.
       -> [a] -- ^Decoded symbols.
decode h [] = []
decode h c0 = decode' h c0 where
    --- XXX reset the tree when emitting a symbol
    decode' (HLeaf s) c = s : decode h c
    decode' (HNode {left = l}) (False : c) = decode' l c
    decode' (HNode {right = r}) (True : c) = decode' r c
    decode' _ _ = error "decode of invalid bitstring"

-- |Transform an HTable back to an HTree.  The HTable must
-- have the property that it will produce an HTree with no
-- empty leaves.  Any HTable produced from an HTree will
-- have this property; (reconstructHTree . makeHTable)
-- should be the identity on HTrees, and (makeHTable
-- . reconstructHTree) should be the identity on HTables.
reconstructHTree :: (Ord a) => HTable a -> HTree a
reconstructHTree (HTable m) = reconstruct . M.toList $ m where
    reconstruct [] = error "makeHTreeFromHTable on ill-formed HTable"
    reconstruct [(sym, [])] = HLeaf sym
    reconstruct t = HNode {depth = tree_depth l `max` tree_depth r,
                           left = l,
                           right = r}
        where
          (l', r') = foldl' split ([], []) t
          (l, r) = (reconstruct l', reconstruct r')
          split (l, r) (sym, bit : bits)
              | bit = (l, (sym, bits) : r)
              | otherwise = ((sym, bits) : l, r)

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
