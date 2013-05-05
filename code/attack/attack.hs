-- Filename: attack.hs
-- Author: Christopher Sasarak

module Main where
import Data.Maybe
import Data.List
import System.Environment
import System.IO

type Pair = (String, String)     

usage = "./attack <plaintext-ciphertext file>"
       
main:: IO ()
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
                
                

