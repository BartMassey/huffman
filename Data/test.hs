import System.Random
import Data.Char
import Data.Function
import Data.List

import Data.Huffman

r :: Int -> Int -> IO [Char]
r k n = do
  as <- rs n
  bs <- rs n
  return . map (chr . (+ ord 'a')) $ zipWith div as bs
  where
    rs :: Int -> IO [Int]
    rs 0 = return []
    rs n = do
      e <- getStdRandom . randomR $ (1, k)
      es <- rs (n - 1)
      return (e : es)

main = do
  l <- r 10 256
  let f = freq ['a'..'k'] l
  let tree = makeHTree (sort f)
  let tab = makeHTable tree
  print tab
