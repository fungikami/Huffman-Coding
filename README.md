## Huffman Coding in Haskell

Huffman coding is a lossless data compression algorithm. The idea is to assign variable-length codes to input characters, lengths of the assigned codes are based on the frequencies of corresponding characters. The most frequent character gets the smallest code and the least frequent character gets the largest code.

### How to run

#### Compile
```
ghc -o ./huffman ./Main.hs
```

#### Encode a example file
```
./huffman -c examples/a.txt
```

#### Decode a example file
```
./huffman -d examples/b.hz
```