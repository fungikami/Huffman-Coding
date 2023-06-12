-- Huffman Coding implementation in Haskell
module HuffmanCoding where

import Data.List as List (sortBy, insertBy)
import Data.Map as Map (fromListWith, toList)
import Data.Binary as Binary

-- Data type for Huffman Tree
-- Leaf contains the frequency and the character
-- Node contains the frequency, the list of characters, and the left and right subtrees
data Tree = Leaf Int Char
            | Node Int [Char] Tree Tree
            deriving (Show, Eq)

-- Make Tree an instance of Binary to be able to encode and decode it in a file
instance Binary.Binary Tree where 
    put :: Tree -> Binary.Put
    put (Leaf f c) = do 
        Binary.putWord8 0
        Binary.put f
        Binary.put c
    put (Node f cs l r) = do
        Binary.putWord8 1
        Binary.put f
        Binary.put cs
        Binary.put l
        Binary.put r

    get :: Binary.Get Tree
    get = do 
        tag <- Binary.getWord8
        case tag of 
            0 -> do 
                f <- Binary.get
                c <- Binary.get
                return (Leaf f c)
            1 -> do 
                f <- Binary.get
                cs <- Binary.get
                l <- Binary.get
                r <- Binary.get
                return (Node f cs l r)

-- Get the frequency of a node 
getFreq :: Tree -> Int
getFreq (Leaf f _) = f
getFreq (Node f _ _ _) = f

-- Get the characters of a node
getChars :: Tree -> [Char]
getChars (Leaf _ c) = [c]
getChars (Node _ cs _ _) = cs

-- Create map of frequencies: occurrences of each character in a string
freqMap :: String -> [(Char, Int)]
freqMap s = Map.toList (Map.fromListWith (+) [(c, 1) | c <- s])

-- Sort map of frequencies 
sortFreqMap :: [(Char, Int)] -> [(Char, Int)]
sortFreqMap = List.sortBy (\(_,a) (_,b) -> compare a b)

-- Create Huffman Tree:
-- Given a string, create a Huffman Tree
createTree :: String -> Tree
createTree s =  let freqs = sortFreqMap (freqMap s)
                    trees = map (\(c, f) -> Leaf f c) freqs
                in createTree' trees

-- Auxiliar function to create Huffman Tree
createTree' :: [Tree] -> Tree
createTree' [] = Leaf 0 ' '             -- Empty node
createTree' [t] = t                     -- Single node
createTree' (t1:t2:ts) =                -- Two or more nodes
    let t' = Node (getFreq t1 + getFreq t2) (getChars t1 ++ getChars t2) t1 t2
    in createTree' (insertTree (t':ts))

-- Insert tree: Given a list of trees, insert a tree in order
insertTree :: [Tree] -> [Tree]
insertTree [] = []
insertTree [t] = [t]
insertTree t = sortBy (\t1 t2 -> compare (getFreq t1) (getFreq t2)) t

-------------------- Encoding / Decoding --------------------

-- Generate binary coding for each character
genBinary :: Tree -> [([Char], Char)]
genBinary (Leaf _ c) = [("1", c)]     -- Single node, path is 1
genBinary (Node _ _ l r) = genBinary' l "0" ++ genBinary' r "1"

-- Auxiliar function to generate binary coding for each character
genBinary' :: Tree -> [Char] -> [([Char], Char)]
genBinary' (Leaf _ c) s = [(s, c)]
genBinary' (Node _ _ l r) s = genBinary' l (s ++ "0") ++ genBinary' r (s ++ "1")

-- Search by character: Given a list of tuples (key, value) and a value,
-- return the key if the value is found, otherwise return Nothing
searchByChar :: Eq val => [(key, val)] -> val -> Maybe key 
searchByChar [] _ = Nothing
searchByChar ((k, v) : s) searchVal | v == searchVal = Just k 
                                    | otherwise = searchByChar s searchVal

-- Encode: Given a message, encode it and return the encoded message and 
-- the Huffman Tree
encode :: String -> (String, Tree)
encode string = 
    let hTree = createTree string
        table = genBinary hTree
    in encodeHelper string table hTree
    where
        encodeHelper [] _ hTree = ("", hTree)
        encodeHelper (c:cs) table hTree =   
            let char = searchByChar table c
            in case char of
                Nothing -> ("", hTree)
                Just char -> let (rest, tree) = encodeHelper cs table hTree
                            in (char ++ rest, tree)

-- Decode: Given a message and a Huffman Tree, decode the message
decode :: (String, Tree) -> String
decode (msg, tree) =    let table = genBinary tree
                        in decodeHelper msg table
                        where
                            decodeHelper [] _ = ""
                            decodeHelper msg table = decode' msg table 1 ""

-- Auxiliar function to decode: 
-- Given a message, a table of binary codes, an index, and a decoded message,
-- decode the message recursively
decode' :: String -> [([Char], Char)] -> Int -> String -> String
decode' [] table i [] = "" 
decode' [] table i decodedMsg = decodedMsg 
decode' msg table i decodedMsg = 
    let text = lookup (take i msg) table
    in case text of
        Nothing -> decode' msg table (i + 1) decodedMsg
        Just decodedChar ->
            decode' (drop i msg) table 1 (decodedMsg ++ [decodedChar])

-- Test
-- main :: IO ()
-- main = do
--     input <- getLine
--     let (encoded, resultTree) = encode input
--     let decoded = decode (encoded, resultTree)
--     putStrLn $ "Encoded message: " ++ encoded
--     putStrLn $ "Decoded message: " ++ decoded
--     return ()
