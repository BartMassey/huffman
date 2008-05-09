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

roundtrip l = decode tree (encode table l) where
    tree = makeHTree HInitNone l
    table = makeHTable tree

main = do
  [arg] <- getArgs
  let h = huffman_data (read arg)
  (putStrLn . show . (== h) . roundtrip) h
