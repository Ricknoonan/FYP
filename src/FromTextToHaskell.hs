module ToHaskell where

import Contract 
import ContractClass

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import System.IO  
import System.Directory  
import Data.List
import Data.Char

main = do
    putStrLn "What contract do you want to load? "
    fileName <- getLine        
    handle <- openFile (file ++ ".txt") ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let contract = (readDataTypes (map (map toLower) (words contents)))
    putStrLn "What do you want to do?>"
			putStrLn "[1] Simulate Contract"
			putStrLn "[2] Generate Solidity"
			input2 <- getLine
			case input2 of
				("1") -> simulate contract
				("2") -> (toFile fileName contract) 
    hClose handle  
    hClose tempHandle 

readDataTypes :: [String] -> Contract
readDataTypes ("end":xs) = End  
readDataTypes ("function":x:xs) = (Function x (readDataTypes xs))
readDataTypes ("constructor":xs) = (Constructor (readDataTypes xs))
readDataTypes ("set":"contractowner":xs) = (Set (ContractOwner) (readDataTypes xs))
readDataTypes ("cashin":"nolimit":xs) = (CashIn (NoLimit) (readDataTypes xs))
readDataTypes ("addto":x:xs) = (AddTo x (readDataTypes xs))
readDataTypes ("Withdraw": xs) = (Withdraw (readDataTypes xs)) 