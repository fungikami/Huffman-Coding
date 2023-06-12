module Main where 

import System.Environment (getArgs)
import System.FilePath (dropExtension)
import qualified Data.Binary as Binary

import HuffmanCoding (encode, decode, Tree)

-- Main function: Huffman coding
-- Usage: huffman [-c|-d] <filename>.[txt|hz]
-- -c: Encode file txt to file hz
-- -d: Decode file hz to file txt
main :: IO ()
main = do 
    args <- getArgs
    case args of
        ["-c", path] -> do
            -- Read file content and encode it
            content <- readFile path
            let (code, tree) = encode content
            
            -- Save content and tree to file .hz 
            let name = dropExtension path
            Binary.encodeFile (name ++ ".hz") (code, tree)

        ["-d", path] -> do
            -- Read file content and decode it
            (code, tree) <- Binary.decodeFile path
            let decoded = decode (code, tree)

            -- Save decoded content to file .txt
            let name = dropExtension path
            writeFile (name ++ ".txt") decoded
        _ -> putStrLn "Usage: huffman [-c|-d] <filename>.[txt|hz]"

