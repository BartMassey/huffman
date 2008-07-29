import Data.List
import Data.Char
import Test.QuickCheck
import Text.Printf

import Data.Huffman


main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
instance Arbitrary Char where
    arbitrary = sized $ \n -> frequency
        [(n `div` i, return $ chr (ord 'a' + i)) | i <- [1 .. n `min` 26]]

prop_encode_decode l = (decode tree . encode table $ l) == id l where
    l' = sort . freq [] $ l :: [Freq Int Char]
    tree = makeHTree l'
    table = makeHTable tree

prop_make_recon l = (reconstructHTree . makeHTable $ t) == id t where
    t = makeHTree . sort . freq [] $ l :: HTree Char

tests = [("decode.encode/id", test prop_encode_decode),
         ("reconstructHTree.makeHTable/id", test prop_make_recon)]
         
