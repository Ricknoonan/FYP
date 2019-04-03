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
    handle <- openFile ("examples\\" ++ fileName ++ ".txt") ReadMode
    contents <- hGetContents handle
    let cleanedContents = removePunc contents
    let contractList = map (map toLower) (toWords cleanedContents)
    let contractType = (readDataTypes contractList)
    putStrLn (show contractList)
    putStrLn (show contractType)
    checkErrors contractType
    checkLogic contractList
    choice fileName contractType contractList
    hClose handle

choice :: String -> Contract -> [String] -> IO ()
choice fileName contractType contractList = do 
    putStrLn ("Contract " ++ fileName ++ " loaded" )
    putStrLn "What do you want to do?>"
    putStrLn "[1] Simulate Contract"
    putStrLn "[2] Generate Solidity"
    putStrLn "[3] Load a different contract"
    input2 <- getLine
    case input2 of
        ("1") -> simulate contractType >> choice fileName contractType contractList
        ("2") -> (toFile fileName contractType) >> putStrLn "Contract Generated Succesfully!"
        ("3") -> main 

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
        (CommitEther i c1) -> checkErrors c1
        (CashBackAll c1) -> checkErrors c1
        (Send s c1) -> checkErrors c1
        (Withdraw c1) -> checkErrors c1
        (Allow m c1) -> checkErrors c1
        (Function s c1) -> checkErrors c1
        (IsNot c1) -> checkErrors c1
        (Set p c1) -> checkErrors c1
        (Constructor c1) -> checkErrors c1
        (AddTo s c1) -> checkErrors c1
        (Unless fc c1) -> checkErrors c1
        (From str c1) -> checkErrors c1
        (End)-> do 
            putStrLn "Contract Types Correct" 

checkLogic :: [String] -> IO () 
checkLogic s 
    | (not (checkWithdraw s)) = do 
        putStrLn "Withdraw Logic not correct"
        putStrLn "Make sure there is a CommitEther contract"
        putStrLn "Make sure there is an AddTo contract"
        main
    | not (checkConstuctor s) = do 
        putStrLn "Constructor Logic not correct"
        putStrLn "Only Set is allowed in Constructor"
        main
    | not (checkPartialRest s) = do 
        putStrLn "Partial PayOption Logic not correct"
        putStrLn "If you are using Partil Pay Option, make sure there a Rest PayOption "
        main
    | not (checkSend s) = do 
        putStrLn "Send Logic not correct "
        putStrLn "Make sure there is a CommitEther Contract"
        main
    | not (checkCommitEther s) = do 
        putStrLn "CommitEther Logic not correct "
        putStrLn "Make sure there is a Withdraw or a Send contract"
        main
    | otherwise = do 
        putStrLn ("Contract Logically Correct")

removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` "(),.?!-:;\"\'") ]

toWords :: String -> [String]
toWords s = filter (\w -> w `notElem` ["they", "are", "an", "can", "there", "the","for"]) (words s)
        