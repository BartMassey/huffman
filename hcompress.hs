--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

import System.Environment
import Data.List
import Data.Huffman
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as B
import Data.Bits
import qualified Data.Map as M

stringify l =
  let (l', ls) = splitAt 8 l
  in
    case length l' of
      8 -> encode_byte l' : stringify ls
      _ -> [encode_byte (l' ++ replicate (8 - length l') False)]
  where
    encode_byte b = snd (foldl' accum_bit (0, 0) (map bitval b))
        where
          bitval True = 1
          bitval False = 0
	  accum_bit (k, a) v =
	    (k + 1, a .|. (v `shiftL` k))

compress instring = outstring where
  m = maxBound :: Word16
  sample_size = 4 * (1 + fromIntegral m)
  sample = (B.unpack . B.take sample_size) instring
  f = freq ([minBound..maxBound]::[Word8]) sample
  f' = recount m f
  tree = makeHTree f'
  table = makeHTable tree
  l = encode table (B.unpack instring)
  outstring = B.pack (stringify l)

main = B.interact compress
