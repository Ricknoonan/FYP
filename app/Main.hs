module Main (
        main
    ) where

import Contract
import ContractClass
import Prelude hiding (until)
import Betting 
import System.IO.Unsafe (unsafePerformIO)



main :: IO ()
main = do 
    control (bettingContract)

control :: Contract -> IO ()
control c = do 
    putStrLn (show c) 
    let res = loop c
    let (nc, no, ns) = evalAll c (unsafePerformIO (res))
    putStrLn (show no)
    if (nc == End) then putStrLn ("Contract finished")
        else control nc
    
loop :: Contract -> IO String
loop c = do 
    case c of 
        (Until obs c1 c2) -> return ("Nothing")
        (CashIn val person c1 c2)  -> do 
                                putStrLn "Enter Input: "
                                input <- getLine 
                                return input
        
        (Pay person1 person2 val c1) -> do 
                                putStrLn "Enter decision: "
                                input <- getLine 
                                return input
        (When obs c1 c2) -> return ("Nothing")
                             

