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
                let sk = [findCorrectSK pairs x | x <- [4,3,2,1]]
                let sk' = map (concat . map (\x -> showHex x "")) $ transpose sk
                putStr $ unlines sk' 
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

-- Find the correct subkeys for byte b in the list of pairs.
findCorrectSK :: [[PCPair]] -> Int -> [Word8]
findCorrectSK pairs b = let sks = [[getSubkey wk p b | p <- pairs] | wk <- possibleKeys]
                            -- Filter out the lists where the same whitening key
                            -- guess yields a different answer
                            valid = map head $ filter (\(x:xs) -> all (== x) xs) sks 
                            ind = elemIndices True $ map (\x -> all (== (head x)) x) sks
                        in 
                          valid

-- Find the a subkey byte from a plaintext ciphertext pair and a given
-- whitening guess. You must specify the subkey to find, [1..4].
getSubkey :: Word8 -> [PCPair] -> Int -> Word8
getSubkey wk pairs keyNo = case keyNo `mod` 2 of
                            0 -> getSubkeyEven wk pairs keyNo
                            1 -> getSubkeyOdd wk pairs keyNo

-- Find the a subkey byte from a plaintext ciphertext pair and a given
-- whitening guess. You must specify the subkey to find, [1..4]. This function
-- is for the 'Even' pairs, e.g. the ones where f0 is used.
getSubkeyEven :: Word8 -> [PCPair] -> Int -> Word8
getSubkeyEven wk pairs keyNo = let b1 = keyNo * 2 - 1 `mod` 8
                                   b2 = keyNo * 2 `mod` 8
                                   ca = snd $ pairs !! b1 -- 0th byte
                                   cb = snd $ pairs !! b2 -- 7th byte
                                   -- Undo the last whitening performed on c
                                   ca' = (ca - wk)
                                   fOut = (f0 cb)
                                   p = fst $ pairs !! b1 
                                   x' = ca' `xor` p
                               in 
                                  (x' - fOut)
                         
-- Find the a subkey byte from a plaintext ciphertext pair and a given
-- whitening guess. You must specify the subkey to find, [1..4]. This function
-- is for the 'Odd' pairs, e.g. the ones where f1 is used.
getSubkeyOdd :: Word8 -> [PCPair] -> Int -> Word8
getSubkeyOdd wk pairs keyNo = let b1 = (7 - (keyNo * 2 - 1 `mod` 8))
                                  b2 = (7 - (keyNo * 2 `mod` 8))
                                  ca = snd $ pairs !! b2 -- 5th byte
                                  cb = snd $ pairs !! b1  -- 6th byte
                                  -- Undo the last whitening performed on c
                                  ca' = (ca `xor` wk)
                                  fOut = f1 cb
                                  p = fst $ pairs !! (b1 `mod` 8)
                                  x' = ca' - p
                              in 
                                 (fOut `xor` x')

-- Performs the f0 function on a byte
--
f0 :: Word8 -> Word8
f0 w = (rotate w 1) `xor` (rotate w 2) `xor` (rotate w 7)
        
-- Performs the f1 function on a byte
f1 :: Word8 -> Word8
f1 w = (rotate w 3) `xor` (rotate w 4) `xor` (rotate w 6)
