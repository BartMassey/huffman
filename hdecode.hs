--- Copyright © 2008 Bart Massey
--- ALL RIGHTS RESERVED
--- [This program is licensed under the "3-clause ('new') BSD License"]
--- Please see the file COPYING in the source
--- distribution of this software for license terms.

import Data.List
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as B

import Data.Huffman

decode_table f = (decode1 0 . B.unpack) f where
    conv b1 b2 = (fromIntegral b1 `shiftL` 8) .|. fromIntegral b2
    decode1 _ [] = []
    decode1 s (b1 : b2 : bs) = toFreq (conv b1 b2, s) : decode1 (s + 1) bs

decode_data s = first_bits ++ last_bits where
    l = B.unpack s
    first_bits = (concatMap decode_byte . init) l
    decode_byte b = foldl decode_bit [] [0..7] where
        decode_bit v n = ((b `shiftR` n) .&. 1 == 1) : v
    last_bits = (concat . init . group . decode_byte . last) l

uncompress instring = outstring where
  (table_string, data_string) = B.splitAt 512 instring
  f = decode_table table_string :: [Freq Word16 Word8]
  tree = (makeHTree . sort) f
  l = decode_data data_string
  outstring = B.pack (decode tree l)

main = B.interact uncompress
