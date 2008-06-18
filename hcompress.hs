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
import Data.Ord
import qualified Data.ByteString.Lazy as B
import Data.Bits
import qualified Data.Map as M

encode_table f = (B.pack . concatMap encode_entry) f' where
    f' = sortBy (comparing snd) (map fromFreq f)
    encode_entry (c, _) = [ fromIntegral ((c `shiftR` 8) .&. 0xff),
                            fromIntegral (c .&. 0xff) ]

encode_data l = (B.pack . encode_bytes) l  where
  encode_bytes l
      | length l' == 8 = encode_byte l' : encode_bytes ls
      | otherwise = [encode_byte (l' ++ replicate (8 - length l') False)]
      where
        (l', ls) = splitAt 8 l
        encode_byte b = snd (foldl' accum_bit (0, 0) (map bitval b)) where
          bitval True = 1
          bitval False = 0
	  accum_bit (k, a) v = (k + 1, a .|. (v `shiftL` k))

compress instring = outstring where
  m = maxBound :: Word16
  sample_size = 4 * (1 + fromIntegral m)
  sample = (B.unpack . B.take sample_size) instring
  f = freq ([minBound..maxBound]::[Word8]) sample
  f' = recount m f
  table_string = encode_table f'
  tree = makeHTree f'
  table = makeHTable tree
  l = encode table (B.unpack instring)
  data_string = encode_data l
  outstring = table_string `B.append` data_string

main = B.interact compress
