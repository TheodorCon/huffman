# Huffman coding in Haskell 

In my first semester of grad school, I had a miniproject that involved implementing [Huffman's coding algorithm](https://en.wikipedia.org/wiki/Huffman_coding) in *Haskell*, as part of my *Programming Paradigms* course. I implemented enough of it to encode any type of file into an encoded version and decode that back into the original file. However, the original version turned out to be very slow. 

Since then, I have been dabling a bit more in Haskell, and this repository is my attempt to optimize and refactor the original code. 

`main-old.hs` is the original file, commented and explained. `main.hs` is the rewritten version. The main two functions of the module are:

_ `toCodeFile` - turns a file an encoded file, given their paths
_ `fromCodeFile` - turns an encoded file into the original file, given their paths

To try it out, run: 

```shell
ghci ./main.hs
toCodeFile <source_file> <target_encoding_file>
fromCodeFile <target_encoding_file> <target_decoded_file>
```