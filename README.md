# Huffman

This is a Haskell module to do [Huffman
coding](http://en.wikipedia.org/wiki/Huffman_coding). It is
intended to be fairly generic and a good base for
experimentation.

Included are file compression and decompression utilities
that exercises the codebase.  Performance is slower than but
comparable to bzip2.  On my home box (Intel Core 4) bzip2
takes 100ms to compress the text of Moby Dick, producing a
389KB file from a 1276KB original.  My hencode utility takes
340ms to compress this file, producing a 740KB compressed
file. Decompression with hdecode takes 190ms seconds, vs
40ms for bzip2.

The plan for this program was to enhance the Huffman code
module such that it ccould be used as a basis for an
implementation of
[inflate/deflate](http://en.wikipedia.org/wiki/DEFLATE)
compression ([RFC
1951](http://www.faqs.org/rfcs/rfc1951.html)), which is
essentially Huffman coding plus [LZ77
compression](http://en.wikipedia.org/wiki/LZ77_and_LZ78).
This, in turn, can easily be wrapped into a ZLIB
implementation
([RFC1950](http://www.faqs.org/rfcs/rfc1950.html)), and will
facilitate an implementation of the [PNG image compression
standard](http://www.w3.org/TR/PNG/) ([RFC
2083](http://www.faqs.org/rfcs/rfc2083.html) describes an
earlier version).

However, it turned out that somebody had already implemented
inflate/deflate in pure Haskell on hackage, which was cool.
Work on the PNG implementation was abandoned long ago.

A good practical reference for Huffman coding tricks is
[Schindler's
webpage](http://www.compressconsult.com/huffman/).
[Adaptive Huffman
coding](http://en.wikipedia.org/wiki/Adaptive_Huffman_coding)
also looks interesting.

## License

This program is licensed under the "3-clause ('new') BSD
License". Please see the file `COPYING` in this distribution
for license terms.
