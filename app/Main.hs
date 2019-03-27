module Main ( 
	main) where 

{-# LANGUAGE ScopedTypeVariables #-}

import Contract
import ContractClass
import Prelude hiding (until, interact)
import Bank 
import Lottery
import ToSolidity 
import Simulate
import ToContract

import System.IO
{--import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import System.Directory  --}
import Data.List
import Data.Char
import Control.Exception

main = do
	putStrLn "What contract do you want to load? "
	fileName <- getLine
	handle <- openFile (fileName ++ ".txt") ReadMode
	contents <- hGetContents handle
	let contract = readDataTypes (map (map toLower) (words contents))
	checkErrors contract
	putStrLn (show contract)
	putStrLn "What do you want to do?>"
	putStrLn "[1] Simulate Contract"
	putStrLn "[2] Generate Solidity"
	input2 <- getLine
	case input2 of
		("1") -> simulate contract
		("2") -> (toFile fileName contract)
	hClose handle


checkErrors :: Contract -> IO ()
checkErrors c = do
    case c of 
    	(Error str) -> do 
    		putStrLn str 
    		main
    	(When p c1) -> checkErrors c1 
    	(And c2 c1) -> checkErrors c1
    	(Or c2 c1) -> checkErrors c1
    	(Until p c1) -> checkErrors c1
    	(CashIn i c1) -> checkErrors c1
    	(CashBackAll c1) -> checkErrors c1
    	(Send s c1) -> checkErrors c1
    	(Withdraw c1) -> checkErrors c1
    	(Allow m c1) -> checkErrors c1
    	(Function s c1) -> checkErrors c1
    	(Not c1) -> checkErrors c1
    	(Set p c1) -> checkErrors c1
    	(Constructor c1) -> checkErrors c1
    	(AddTo s c1) -> checkErrors c1
    	(Unless fc c1) -> checkErrors c1
    	(End)-> do 
    	    putStrLn "Contract loaded succesfully!" 


{--
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

--}