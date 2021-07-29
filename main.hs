module Main where

import Data.Bifunctor
import Data.Char
import Data.List (find, sortOn)
import System.IO

-- import qualified Data.ByteString as BS

main = undefined

data HuffTree a = Tree (HuffTree a) (HuffTree a) | Leaf a
  deriving (Show, Eq)

type HuffKey = [(Char, String)]

type HuffCode = String

toCountBag :: String -> [(Char, Int)]
toCountBag [] = []
toCountBag str = toCountBagHelper str []
  where
    toCountBagHelper [] acc = acc
    toCountBagHelper (x : xs) acc = toCountBagHelper (filter (/= x) xs) ((x, count) : acc)
      where
        count = length . filter (== x) $ (x : xs)

toHuffTree :: String -> HuffTree Char
toHuffTree str =
  let rawCountBag = toCountBag str
      countBag = sortOn snd rawCountBag
   in toTreeHelper . map (first Leaf) $ countBag
  where
    toTreeHelper [] = error "this should not have happened"
    toTreeHelper [x] = fst x
    toTreeHelper (x : y : xs) = toTreeHelper $ sortOn snd $ bimap (Tree (fst x)) (snd x +) y : xs

toEncodingBag :: String -> [(Char, String)]
toEncodingBag str =
  let huffTree = toHuffTree str
   in toCodesHelper huffTree ""
  where
    toCodesHelper (Leaf x) pref = [(x, pref)]
    toCodesHelper (Tree x y) pref = toCodesHelper x (pref ++ "0") ++ toCodesHelper y (pref ++ "1")

toCode :: String -> (HuffKey, HuffCode)
toCode str =
  let encoding = toEncodingBag str
      maybes = map (\x -> snd <$> find (\y -> x == fst y) encoding) str
   in case sequenceA maybes of
        Just result -> (encoding, concat result)
        Nothing -> error "toCode: could not encode"

-- todo: `show` messes with the encoding... please use bytestring
toCodeFile :: FilePath -> FilePath -> IO ()
toCodeFile input output = withFile input ReadMode $ \file -> do
  rawString <- hGetContents file
  -- hClose file
  let (key, code) = toCode rawString
      (byteCode, rest) = fromBits code
      finalString = show (key, byteCode, rest)
   in withFile output WriteMode $ \outputHandle -> do
        -- outputHandle <- openFile output WriteMode
        hSetEncoding outputHandle latin1
        hPutStr outputHandle $ show key ++ "<break>" ++ byteCode ++ "<break>" ++ rest
        hClose outputHandle

fromCode :: HuffKey -> HuffCode -> String
fromCode key code = matchCode code []
  where
    matchCode [] acc = acc
    matchCode code acc = case find (\(_, seq) -> take (length seq) code == seq) key of
      Just (char, seq) -> matchCode (drop (length seq) code) (acc ++ [char])
      Nothing -> error "\n\tfromCode:\n\tcharacter not found in key"

fromCodeFile :: FilePath -> FilePath -> IO ()
fromCodeFile input output = withFile input ReadMode $ \file -> do
  hSetEncoding file latin1
  keyAndCode <- hGetContents file
  let [stringKey, code, rest] = splitBy keyAndCode "<break>"
      key = read stringKey :: HuffKey
      wholeCode = toBits code rest
   in do
        writeFile output (fromCode key wholeCode)

fromBits :: String -> (String, String)
fromBits string =
  let tokens = split 8 string
      bytes = filter (\x -> length x == 8) tokens
      remainder = if length (last tokens) < 8 then last tokens else ""
   in (map (chr . fromBinaryString) bytes, remainder)

toBits :: String -> String -> String
toBits str rest =
  let stringBits = concatMap (toBinaryString . ord) str
   in stringBits ++ rest

fromBinaryString :: String -> Int
fromBinaryString string =
  if any (`notElem` "01") string
    then error "binary string contains illegal characters"
    else foldl foldHelper 0 $ zip [0 ..] (reverse string)
  where
    foldHelper acc (position, bit) = acc + 2 ^ position * read [bit] :: Int

toBinaryString :: Int -> String
toBinaryString num = addZeros $ decToBits num []
  where
    decToBits 0 acc = reverse acc
    decToBits num acc = decToBits (div num 2) (acc ++ show (mod num 2))
    addZeros str =
      let necessary = 8 - length str
       in if length str > 8
            then error "invalid byte string"
            else concat (replicate necessary "0") ++ str

split :: Int -> String -> [String]
split = splitHelper
  where
    splitHelper _ [] = []
    splitHelper n s = take n s : splitHelper n (drop n s)

splitBy :: String -> String -> [String]
splitBy str del = splitHelper str del []
  where
    splitHelper [] _ acc = [acc]
    splitHelper s d acc =
      let delLength = length d
       in if take delLength s == d
            then acc : splitHelper (drop delLength s) d []
            else splitHelper (tail s) d (acc ++ [head s])