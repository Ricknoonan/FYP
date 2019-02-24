module Main (
        main
    ) where

import Contract
import ContractClass
import Prelude hiding (until)
import Betting 
import CrowdFunding



main :: Contract -> IO ()
main c = do 
    putStrLn 
    loop bettingContract emptyPState emptyCState

loop :: Contract -> ParamState -> ContractState -> IO ()
loop c pst st = do
    case c of 
        (CashIn val c1) -> do   
                                putStrLn "What is your wallet address? > "
                                address <- getLine
                                putStrLn (show address ++ " Commit " ++ show val ++ " >")
                                money <- getLine
                                let moneyIn = (read money :: Money) 
                                let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st 
                                putStrLn (show no ++ show ns)
                                loop nc nco ns 

        (Send obs c1) -> do 
                                putStrLn "Enter decision > "
                                decision <- getLine 
                                let dec = (read decision :: Int)
                                let (nc, no, ns, nco) = run c (Decision dec) pst st 
                                putStrLn (show no)
                                loop nc nco ns
                                
        (Initiate c1) -> do 
            putStrLn "Initiate contract, enter owner address: >"
            address <- getLine 
            let (nc, no, ns, nco) = run c (SetOwner address) pst st 
            putStrLn (show no)
            loop nc nco ns

        (End) -> putStrLn ("Contract finished")

        c -> do 
            let (nc, no, ns, nco) = run c (Empty) pst st 
            loop nc nco ns 