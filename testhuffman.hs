import Huffman

-- ruler function
ruler :: Int -> [Int]
ruler 1 = [1]
ruler n =  l ++ [n] ++ l where
    l = ruler (n - 1)

huffman_data :: Int -> [Int]
huffman_data n = take n (ruler i) where
    i = ceiling (1 + (logBase 2 (fromIntegral n)))

