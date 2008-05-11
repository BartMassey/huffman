--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

import System.Environment
import Data.Huffman
import Data.Bits

-- improved ruler function
ruler :: Int -> [Int]
ruler n = lowbit 0 : ruler (n + 1) where
    lowbit i
        | (n .&. bit i) /= 0 = i
        | otherwise = lowbit (i + 1)

roundtrip n l = decode tree (encode table l) where
    f = freq [] (take n (ruler 1))
    tree = makeHTree f
    table = makeHTable tree

main = do
  [arg] <- getArgs
  let n = read arg
  let h = take n (ruler 1)
  (putStrLn . show . (== h) . roundtrip n) h
