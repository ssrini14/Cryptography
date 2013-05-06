-- Filename: attack.hs
-- Author: Christopher Sasarak

module Main where
import Data.Maybe
import Data.List
import Data.Bits
import Data.Word
import Numeric
import System.Environment
import System.IO

type Pair = (String, String)     
type PCPair = (Word8, Word8)

usage = "./attack <plaintext-ciphertext file>"
       
main :: IO ()
main = do args <- getArgs
          if null args then 
            putStrLn usage
          else
            (do h <- openFile (head args) ReadMode
                contents <- hGetContents h
                let pairs = loadPairs contents
                let sk = findCorrectSK pairs
                putStrLn (showHex sk "")
                -- sequence_ $ map (\x -> do putStr (fst x) 
                --                           putStr ","
                --                           putStrLn (snd x)) pairs 
            )


loadPairs :: String -> [[PCPair]] 
loadPairs inp = map (makePCPairs . parsePair) $ lines inp   

-- Parses a pair out of an individual line of text
parsePair :: String -> Pair
parsePair str = let (x, y) = span (/= ',') str 
                in
                (x, tail y)
                
-- A list of all possible whitening keys
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

findCorrectSK :: [[PCPair]] -> Word8
findCorrectSK pairs = let sks = [[getSubkey wk p | p <- pairs] | wk <- possibleKeys]
                          valid = [all (== (head sk)) sk | sk <- sks] 
                          Just ind = elemIndex True valid
                      in 
                      head (sks !! ind)
                      

-- Verify that this subkey is correct across all plaintext/ciphertext pairs
--verifySK3 :: Word8 -> [[PCPair]] -> Bool
--verifySK3 sk ps = all (== sk) [getSubkey sk x | x <- ps]


-- Find the first subkey byte from a plaintext ciphertext pair and a given
-- whitening guess. You must specify the subkey to find, [1..4].
getSubkey :: Word8 -> [PCPair] -> Word8
getSubkey wk pairs = let c0 = snd $ last pairs
                         x7 = snd $ head pairs
                         -- Undo the last whitening performed on c0
                         c0' = (c0 - wk)
                         -- What came out of f0 for X6 0f the plaintext
                         fOut = f0 x7
                         p7 = fst $ head pairs
                         x0' = c0' `xor` p7
                         in 
                          (fOut - x0')
                         

-- Performs the f0 function on a byte
f0 :: Word8 -> Word8
f0 w = (rotate w 1) `xor` (rotate w 2) `xor` (rotate w 7)
        
-- Performs the f1 function on a byte
f1 :: Word8 -> Word8
f1 w = (rotate w 3) `xor` (rotate w 4) `xor` (rotate w 6)


