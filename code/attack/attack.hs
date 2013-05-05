-- Filename: attack.hs
-- Author: Christopher Sasarak

module Main where
import Data.Maybe
import Data.List
import Numeric
import System.Environment
import System.IO

type Pair = (String, String)     
type PCPair = (Int, Int)

usage = "./attack <plaintext-ciphertext file>"
       
main :: IO ()
main = do args <- getArgs
          if null args then 
            putStrLn usage
          else
            (do h <- openFile (head args) ReadMode
                contents <- hGetContents h
                let pairs = loadPairs contents
                sequence_ $ map (\x -> do putStr (fst x) 
                                          putStr ","
                                          putStrLn (snd x)) pairs 
            )


loadPairs :: String -> [Pair] 
loadPairs inp = map parsePair $ lines inp   

-- Parse's a pair out of an individual line of text
parsePair :: String -> Pair
parsePair str = let (x, y) = span (not . (== ',')) str 
                in
                (x, tail y)
                
-- A list of all possible subkeys
possibleKeys = [0..255]

-- This function will make plain-text cipher-text byte from a Pair for testing
-- It assumes the input Pair strings are in hexadecimal format
makePCPairs :: Pair -> [PCPair]
makePCPairs (l, r) = let parseHex = fst . head . readHex 
                         makeTwos = (map parseHex) . unfoldr (\x -> let (a, b) = splitAt 2 x 
                                                                    in 
                                                                    if null a then Nothing
                                                                    else Just (a, b))
                                                                
                         rs = makeTwos r
                         ls = makeTwos l
                         in
                             zip ls rs



