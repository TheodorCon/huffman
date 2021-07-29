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

testText = "I want to die :)"

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
    toTreeHelper [x] = fst x
    toTreeHelper (x : y : xs) = toTreeHelper $ sortOn snd $ (Tree (fst x) (fst y), snd x + snd y) : xs

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
        Nothing -> error "ur mom geh"

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
        hPutStr outputHandle $ "(" ++ show key ++ ", " ++ byteCode ++ ", " ++ show rest ++ ")"
        hClose outputHandle

fromCode :: HuffKey -> HuffCode -> String
fromCode key code = matchCode code []
  where
    matchCode [] acc = acc
    matchCode code acc = case filter (\(_, seq) -> take (length seq) code == seq) key of
      [(char, seq)] -> matchCode (drop (length seq) code) (acc ++ [char])
      [] -> error "character not found in key... weird"
      _ -> error "something's fucky"

fromCodeFile :: FilePath -> FilePath -> IO ()
fromCodeFile input output = withFile input ReadMode $ \file -> do
  keyAndCode <- hGetContents file
  let (key, code) = read keyAndCode :: (HuffKey, HuffCode)
   in writeFile output (fromCode key code)

fromBits :: String -> (String, String)
fromBits string =
  let tokens = split 8 string
      bytes = filter (\x -> length x == 8) tokens
      remainder = if length (last tokens) < 8 then last tokens else ""
   in (map (chr . fromBinaryString) bytes, remainder)

fromBinaryString :: String -> Int
fromBinaryString string =
  if any (`notElem` "01") string
    then error "binary string contains illegal characters"
    else foldl foldHelper 0 $ zip [0 ..] (reverse string)
  where
    foldHelper acc (position, bit) = acc + (2 ^ position) * read [bit] :: Int

split :: Int -> String -> [String]
split = splitHelper
  where
    splitHelper _ [] = []
    splitHelper n s = take n s : splitHelper n (drop n s)