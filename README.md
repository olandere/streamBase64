streamBase64
============

A stream-based base64 encoder/decoder for Scala.  It provides these operations:
encode(Stream[Byte]): Stream[Char]
decode(Stream[Char]): Stream[Byte]

By using streams, the data to be encoded/decoded does not have to fit into memory.  These operations could also be inserted into a stream processing pipeline.
