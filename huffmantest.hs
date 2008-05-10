--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

import System.Environment
import Huffman

-- ruler function
ruler :: Int -> [Int]
ruler 1 = [1]
ruler n =  l ++ [n] ++ l where
    l = ruler (n - 1)

huffman_data :: Int -> [Int]
huffman_data n = take n (ruler i) where
    i = ceiling (1 + (logBase 2 (fromIntegral n)))

roundtrip n l = decode tree (encode table l) where
    f = freq HInitNone (huffman_data n)
    tree = makeHTree f
    table = makeHTable tree

main = do
  [arg] <- getArgs
  let n = read arg
  let h = huffman_data n
  (putStrLn . show . (== h) . roundtrip n) h
