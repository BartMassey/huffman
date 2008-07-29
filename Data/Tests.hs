import Data.List
import Data.Char
import Test.QuickCheck
import Text.Printf

import Data.Huffman


main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
instance Arbitrary Char where
    arbitrary = sized $ \n -> frequency
        [(n `div` i, return $ chr (ord 'a' + i)) | i <- [1 .. n `min` 26]]

prop_make_recon l = (reconstructHTree . makeHTable $ t) == id t where
    t = makeHTree . sort . freq [] $ l :: HTree Char

tests = [("reconstructHTree.makeHTable/id", test prop_make_recon)]
