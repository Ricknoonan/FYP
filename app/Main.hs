module Main ( 
    main) where 

import Contract
import ContractClass
import Prelude hiding (until, interact)
import Bank 
import Lottery
import ToSolidity 
import Simulate
import ToContract

import System.IO
import Data.List
import Data.Char
import Control.Exception

main :: IO ()
main = do
    putStrLn "What contract do you want to load? "
    fileName <- getLine
    handle <- openFile (fileName ++ ".txt") ReadMode
    contents <- hGetContents handle
    let contractList = map (map toLower) (words contents)
    let contractType = (readDataTypes contractList)
    putStrLn (show contractList)
    putStrLn (show contractType)
    checkErrors contractType
    checkLogic contractList
    choice fileName contractType contractList
    hClose handle

choice :: String -> Contract -> [String] -> IO ()
choice fileName contractType contractList = do 
    putStrLn "What do you want to do?>"
    putStrLn "[1] Simulate Contract"
    putStrLn "[2] Generate Solidity"
    input2 <- getLine
    case input2 of
        ("1") -> simulate contractType >> choice fileName contractType contractList
        ("2") -> (toFile fileName contractType) >> putStrLn "Contract Generated Succesfully!"

checkErrors :: Contract -> IO ()
checkErrors c = do
    case c of 
        (Error str) -> do 
            putStrLn ("Error in Contract: " ++ str)
            putStrLn ("Please edit contract and reload")
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
            putStrLn "Contract Types Correct" 

checkLogic :: [String] -> IO () 
checkLogic s 
    | (checkWithdraw s) && (checkConstuctor s) && (checkPartialRest s) 
        && (checkSend s)   = do 
        putStrLn ("Contract Logically Correct")
    | otherwise = do 
        putStrLn ("Contract Logic Incorrect")
        main 