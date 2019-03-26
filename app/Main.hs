module Main ( 
	main) where 

import Contract
import ContractClass
import Prelude hiding (until, interact)
import Bank 
import Lottery
import ToSolidity 
import Simulate
import System.IO

main :: IO ()
main = do 
	putStrLn "What contract would you like to load?> "
	input1 <- getLine
	case input1 of
		("bank")-> do
			putStrLn "What do you want to do?>"
			putStrLn "[1] Simulate Contract"
			putStrLn "[2] Generate Solidity"
			input2 <- getLine
			case input2 of
				("1") -> simulate bank
				("2") -> (toFile "Bank" bank) 
		("lottery") -> do 
			putStrLn "What do you want to do?>"
			putStrLn "[1] Simulate Contract"
			putStrLn "[2] Generate Solidity"
			input2 <- getLine
			case input2 of
				("1") -> simulate lottery
				("2") -> toFile "Lottery" lottery
		_ -> do 
			putStrLn "Invalid Entry"
			main 