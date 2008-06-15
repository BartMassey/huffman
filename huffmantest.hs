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

toMaybe False _ = Nothing
toMaybe True v = Just v

roundtrip n =
    toMaybe (l == l') n'
    where
      l = take n (ruler 1)
      f = freq [] l
      tree = makeHTree f
      table = makeHTable tree
      el = encode table l
      n' = length el
      l' = decode tree el

main = do
  [arg] <- getArgs
  let n = read arg
  case roundtrip n of
    Nothing -> putStrLn "Failed"
    Just n' -> putStrLn (show n')
