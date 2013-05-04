-- Filename: attack.hs
-- Author: Christopher Sasarak

module Main where
import System.Environment

usage = "./attack <plaintext-ciphertext file>"
       
main:: IO ()
main = do args <- getArgs
          if null args then 
            putStrLn usage
          else
            putStrLn "Do stuff here!" 
        
loadAndStore args = "hello"
