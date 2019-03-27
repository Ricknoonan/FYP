module Simulate where

import Contract
import ContractClass
import Prelude hiding (until)
import Data.Map (Map, (!))
import qualified Data.Map as Map

simulate :: Contract -> IO ()
simulate c = do 
    loop c emptyPState emptyCState

loop :: Contract -> ParamState -> ContractState -> IO ()
loop c pst st = do
    case c of 
        (CashIn (Equal val) c1) -> do   
                                putStrLn "What is your wallet address? > "
                                address <- getLine
                                putStrLn (show address ++ " Commit " ++ show val ++ " >")
                                money <- getLine
                                let moneyIn = (read money :: Money) 
                                let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st 
                                prettyPrint no ns
                                loop nc nco ns 
        (CashIn (NoLimit) c1) -> do
                                putStrLn "Simulating CashIn > "   
                                putStrLn "What is your wallet address? > "
                                address <- getLine
                                putStrLn (show address ++ " Commit Any Amount >")
                                money <- getLine
                                let moneyIn = (read money :: Money) 
                                let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st 
                                putStrLn ("Contract State after CashIn: ")
                                prettyPrint no ns
                                loop nc nco ns 

        (Send (Winner All) c1) -> do 
                                putStrLn "Enter decision > "
                                decision <- getLine 
                                let dec = (read decision :: Int)
                                let (nc, no, ns, nco) = run c (Decision dec) pst st 
                                putStrLn (show no)
                                loop nc nco ns
        (Withdraw c1) -> do 
            putStrLn "Simulating Withdraw > "
            putStrLn "Choose Wallet >"
            wallet <- getLine 
            putStrLn "Withdraw Amount >"
            amount <- getLine
            let wal = (read wallet :: Int)
            let amnt = (read amount :: Money)
            let (nc, no, ns, nco) = run c (WithdrawEther (Decision wal) (Amount amnt)) pst st
            putStrLn ("Contract State after Withdraw: ")
            prettyPrint no ns
            loop nc nco ns

        (Set (ContractOwner) c1) -> do
            putStrLn "Simulating Set ContractOwner >"
            putStrLn "What is your wallet address? > "
            address <- getLine
            let (nc, no, ns, nco) = run c (SetOwner address) pst st
            putStrLn ("Contract State after Set ContractOwner: ")
            prettyPrint no ns
            loop nc nco ns 

        (End) -> putStrLn ("Contract finished")

        c -> do 
            let (nc, no, ns, nco) = run c (Empty) pst st 
            loop nc nco ns 

prettyPrint :: OP -> ContractState -> IO ()
prettyPrint o const = do 
                      putStrLn ("Output: " ++ niceOutput o) 
                      putStrLn ("Commits: "++ show(getCommitAction (commitSize const) const))
                      putStrLn ("Money Out: " ++ show (getWithdrawAction (withdrawSize const) const))
                      putStrLn ("Contract Balance: " ++ show (etherBalance const))
                      putStrLn ("Owner: " ++ show (owner const))

getCommitAction :: Int -> ContractState -> [[(Int, Action)]]
getCommitAction 0 const = []
getCommitAction n const = [(n, ((commits const) ! n))] : getCommitAction (n-1) const

getWithdrawAction :: Int -> ContractState -> [[(Int, Action)]]
getWithdrawAction 0 const = []
getWithdrawAction n const = [(n, ((withdrawls const) ! n))] : getWithdrawAction (n-1) const

niceOutput :: OP -> String
niceOutput [x] = (show x)