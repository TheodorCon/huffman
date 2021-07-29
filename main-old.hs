import Data.List
import Data.Char
import System.IO


--Name: Theodor Constantin
--AAU study number: 20191030 
--AAU email: tconst19@student.aau.dk

-- DEMONSTRATIONAL DECLARATIONS

-- these are just mock texts for testing, just for the ease of use and demonstration
lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua." 
bigLorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
veryBigLorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Vitae et leo duis ut diam quam nulla porttitor. Turpis egestas integer eget aliquet nibh praesent tristique magna. Odio ut enim blandit volutpat maecenas. Sem nulla pharetra diam sit amet nisl suscipit adipiscing. Faucibus nisl tincidunt eget nullam. Libero justo laoreet sit amet cursus. Pellentesque diam volutpat commodo sed egestas egestas fringilla. Blandit volutpat maecenas volutpat blandit aliquam etiam. Est lorem ipsum dolor sit. Tellus orci ac auctor augue mauris augue neque gravida in. Vehicula ipsum a arcu cursus vitae congue. Viverra suspendisse potenti nullam ac tortor vitae. Consectetur libero id faucibus nisl tincidunt eget. Penatibus et magnis dis parturient. Fames ac turpis egestas maecenas. Ultricies mi eget mauris pharetra et ultrices neque ornare aenean. Amet consectetur adipiscing elit duis tristique sollicitudin. Dapibus ultrices in iaculis nunc sed augue lacus viverra. Velit euismod in pellentesque massa placerat. Lacus viverra vitae congue eu. Egestas purus viverra accumsan in nisl. Orci porta non pulvinar neque laoreet suspendisse interdum consectetur libero. Id leo in vitae turpis massa. Id neque aliquam vestibulum morbi blandit. Nulla facilisi etiam dignissim diam quis enim lobortis. Ac tortor dignissim convallis aenean et tortor at risus. Blandit cursus risus at ultrices mi tempus. In hac habitasse platea dictumst vestibulum rhoncus est pellentesque. In nibh mauris cursus mattis molestie a iaculis. Ut porttitor leo a diam sollicitudin tempor. Rhoncus urna neque viverra justo nec ultrices. Egestas sed sed risus pretium quam vulputate dignissim. Integer malesuada nunc vel risus commodo. Diam donec adipiscing tristique risus nec feugiat. Habitant morbi tristique senectus et netus et malesuada. Iaculis at erat pellentesque adipiscing commodo elit at. Sollicitudin aliquam ultrices sagittis orci a scelerisque. Diam vulputate ut pharetra sit amet aliquam id diam. Gravida rutrum quisque non tellus orci ac. Mi sit amet mauris commodo quis imperdiet massa. Viverra maecenas accumsan lacus vel. Et netus et malesuada fames. Ultricies tristique nulla aliquet enim. Et malesuada fames ac turpis egestas sed tempus urna et. Posuere ac ut consequat semper viverra nam libero justo laoreet. Nec nam aliquam sem et. Sed turpis tincidunt id aliquet risus feugiat in ante. Commodo elit at imperdiet dui accumsan sit. Lacus vestibulum sed arcu non odio euismod lacinia at quis. Cras adipiscing enim eu turpis egestas pretium aenean. Sodales ut eu sem integer vitae justo eget magna. Nisi quis eleifend quam adipiscing vitae proin sagittis nisl rhoncus. Tortor at auctor urna nunc id cursus metus aliquam. Pharetra pharetra massa massa ultricies mi quis hendrerit. Et pharetra pharetra massa massa ultricies mi quis. Tincidunt dui ut ornare lectus sit."

--default file IO directories
--note that these are dependant of the current directory of your GHCI
inFile = "./haskell/input.txt"
outFile = "./haskell/output.txt"
inOutFile = "./haskell/decodedOutput.txt"

--rencode is demonstrational... it encodes the contents of the default 'inFile' input directory, and writes them into the default 'outFile' output directory
--note that rencode will create an output file if it does not exist
rencode = readAndSaveReduced inFile outFile

-- reddecode is demonstrational and it reads an encoding from the default output file, decodes it and writes the result in the inOutFile (the third file declaration at the top of this program)
reddecode = readAndDecodeReduced outFile inOutFile

-- you may run main for a demonstration
main = do 
       --these will write in files 
       rencode 
       reddecode
       --these will print in console 
       let code = encode veryBigLorem in do 
              print code
              print (decode code)

-- CORE PROGRAM BEGINS HERE

-- occurences::(Eq a) => [a] -> [(a, Int)] -> [(a, Int)] 
occurences (x:xs) acc = 
       if not (any (\el -> x == (fst el)) acc)
              then occurences xs (acc ++ [(x , length (filter (\el -> el == x) (x:xs)))])
              else occurences xs acc
occurences [] acc = acc

data Tree = Leaf Int Char | Branch Int Tree Tree | Null
       deriving (Show, Read)

treeComp (Leaf a _) (Leaf b _) = a < b
treeComp (Branch a _ _) (Branch b _ _) = a < b
treeComp (Leaf a _) (Branch b _ _) = a < b
treeComp (Branch a _ _) (Leaf b _)= a < b
treecomp Null Null = True
treecomp _ Null = False
treecomp Null _ = True

-- occurencesToLeaves:: [(Char, Int)] -> [Tree]
occurencesToLeaves (x:xs) = [(Leaf (snd x) (fst x))] ++ occurencesToLeaves xs
occurencesToLeaves [] = []

-- function that adds two trees together by putting them in a superior branch with a combined weight 
treeAdd::Tree -> Tree -> Tree
treeAdd (Leaf a t) (Leaf b s)= Branch (a + b) (Leaf a t) (Leaf b s)
treeAdd (Branch a t1 t2) (Leaf b s)= Branch (a + b) (Branch a t1 t2) (Leaf b s)
treeAdd (Leaf a t) (Branch b s1 s2) = Branch (a + b) (Leaf a t) (Branch b s1  s2)
treeAdd (Branch a t1 t2) (Branch b s1 s2)= Branch (a + b) (Branch a t1 t2) (Branch b s1 s2)

getWeight::Tree -> Int 
getWeight (Leaf a _) = a 
getWeight (Branch a _ _) = a 
getWeight Null = 0

-- leavesToTree processes a list of leaves into a Huffman binary tree
leavesToTree::[Tree] -> Tree
leavesToTree (x:y:xs) = leavesToTree (sortOn getWeight ([treeAdd x y] ++ xs))
leavesToTree ((Leaf a t):[]) = (Branch a (Leaf a t) Null)
leavesToTree ((Branch a t1 t2):[]) = (Branch a t1 t2)
leavesToTree [] = Null

-- getBits returns a bit encoding for a character based on a Huffman binary tree 
getBits::Char -> Tree -> [Char] -> [Char]
getBits ch (Branch a t1 t2) acc = getBits ch t1 (acc ++ "0") ++ getBits ch t2 (acc ++ "1") 
getBits ch (Leaf a c) acc = if ch == c
                                   then acc
                                   else ""
getBits ch Null acc = ""
-- getAssociationList maps a list of occurences to a bit representation to character association list
getAssociationList tree occ = map (\x -> ((fst x) , (getBits (fst x) tree ""))) occ

-- findBits retreives the Char - bit-representation association tuple from an association list
findBits assoc ch = head (filter (\x -> (fst x) == ch) assoc)

-- EncodingOutput is the data type I used for representing my encoding output
data EncodingOutput = Output [(Char, [Char])] [Char]
                                          deriving (Show, Read)

--encode is the function that wraps everything together and produces an encoding output based on a text
--the EncodingOutput contains athe encoding alongside the bit association list
encode::[Char] -> EncodingOutput
encode text = let occ = (occurences text [])
                  leaves = (occurencesToLeaves occ)
                  tree = (leavesToTree leaves)
                  bm = (getAssociationList tree occ)
              in (Output bm (foldl (\ x y-> x ++ y) "" (map (\x -> (snd (findBits bm x))) text)))

-- decode is the function that reverses the encoding process and produces the decoded text
-- note that I decode using an association list and not a Hufmann tree
decode::EncodingOutput -> [Char]
decode (Output assoc bits) = decodeHelper bits assoc ""                     
decodeHelper "" _ acc = acc
decodeHelper cd bm acc = let bitsPair = (head (filter (\x -> (snd x) == take (length (snd x)) cd) bm)) in
                            decodeHelper (drop (length (snd bitsPair)) cd) bm (acc ++ [(fst bitsPair)])

-- HERE ENDS THE CORE PROGRAM

-- FILE IO AND REDUCED ENCODING BEGIN HERE

-- the following two function handle encoding from and to a file and decodeing from an encoded output file 
-- NOTE: read and decode only works with the output produced by readAndEncode
-- readAndEncode works with text files
readAndDecode::FilePath -> IO [Char]
readAndDecode inputFile = do 
       s <- readFile inputFile
       return (decode (read s))

readAndEncode::FilePath -> FilePath -> IO () 
readAndEncode inputFile outputFile = do 
       s <- readFile inputFile
       writeFile outputFile (show (encode s))

-- bitToString converts binary strings into their decimal Int equivalents
bitStringToDec::Num a => [Char] -> a
bitStringToDec (x:xs) = if x == '0'
                            then bitStringToDec xs
                            else 2 ^ (length xs) + bitStringToDec xs
bitStringToDec [] = 0

-- IntToBotString converts a decimal Int into its binary equivalent 
decToBitString::(Integral a, Show a) => a -> [Char]
decToBitString num = decToBitStringHelper num []
decToBitStringHelper 0 acc = reverse acc 
decToBitStringHelper num acc = decToBitStringHelper (div num 2) (acc ++ (show (mod num 2)))

-- bitStringToByte turns bitstrings shorter that 8 Chars into byte-strings, by adding '0's to the beginning of the string 
-- if the string is longer that 8 Chars, then it is simply returned
bitStringToByte bits = if (length bits) < 8
                            then bitStringToByte ('0' : bits)
                            else bits

-- flaten concatenates lists within a list and returns the result as one list of elements  
flatten::[[t]] -> [t]
flatten [] = []
flatten stringList = (head stringList) ++ (flatten (tail stringList))

--reducedTextToBits maps a list of ASCII characters into a list of string-bytes, and returns it as a flattened bit string 
-- this is used for decoding
reducedTextToBits text = let 
       asciiInts = map ord text
       bitsList = map decToBitString asciiInts
       byteList = map bitStringToByte bitsList
       in flatten byteList

-- ReducedEncoding is the data type that represents my ASCII encoding of a Hufmann code 
data ReducedEncoding = Reduced [Char] [Char]
                            deriving (Show, Read)
                            
-- bitTextToCharList processes a string of bits into an ASCII Reduced encoding, where the first part is the ASCII equivalent of the bit-string and the second part is the remainder bits, due to the original bit-string not dividing perfectly into bytes 
bitTextToCharList text acc = if (length text) >= 8
                                   then bitTextToCharList (drop 8 text) (acc ++ [bitStringToDec (take 8 text)])
                                   else Reduced (map (\x -> (chr x)) acc) text

reducedString (Reduced string rest) = string
reducedRest (Reduced string rest) = rest


-- readAndSAveReduced reads from the file of which path is specified in the input parameter and encodes the contents into the file of which path is specified in the output parameter
readAndSaveReduced input output = do
       s <- readFile input 
       encodeHuffmanToAsciiInFile (encode s) output 

encodeHuffmanToAsciiInFile (Output assoc bits) output = do
       withFile output WriteMode (\f -> do 
              hSetEncoding f latin1
              hPutStr f ((show assoc) ++ "<break>" ++ (reducedToString (bitTextToCharList bits []))))

--reducedToString inserts the serialization tag in between the parts of thee reduced encoding, for file input
reducedToString (Reduced a b) =  a ++ "<break>" ++ b

first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a

--readAndDecodeReduced will read the encoding from the input file (as a FilePath) and write the decoded text into the output file (as a FilePath)
readAndDecodeReduced input output = do
       fileHandle <- openFile input ReadMode
       hSetEncoding fileHandle latin1
       s <- hGetContents fileHandle
       writeFile output (decodeReducedTuple (parseSaved s))

-- decodeReducedTuple decodes an encoding presented as a tuple of a bit association list, an ASCII reduced encoding and a bit remainder string 
decodeReducedTuple tuple = decode (Output (first tuple) ((reducedTextToBits (second tuple)) ++ (third tuple)))

-- parseSaved parses the serialization of an encoding tuple read from a file into an encoding three-tuple of a bit association list, an ASCII reduced encoding and a bit remainder string
parseSaved str = parseSavedHelper str ""
parseSavedHelper str map = if (take 7 str) == "<break>"
                                   then parseSavedHelper2 (drop 7 str) map ""
                                   else parseSavedHelper (tail str) (map ++ [(head str)])
parseSavedHelper2 str map code =  if (take 7 str) == "<break>"
                                          then ((read map::([(Char, [Char])])), code, (drop 7 str))
                                          else parseSavedHelper2 (tail str) map (code ++ [(head str)])

