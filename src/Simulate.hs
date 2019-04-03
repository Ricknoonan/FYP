module Simulate where

import Contract
import ContractClass
import Prelude hiding (until)
import Data.Map (Map, (!))
import qualified Data.Map as Map

simulate :: Contract -> IO ()
simulate c = do 
    putStrLn "Press any key to step through contract"
    loop c emptyPState emptyCState

loop :: Contract -> ParamState -> ContractState -> IO ()
loop c pst st = do
    case c of 
        (CommitEther (Min val) c1) -> do
            wait <- getLine
            putStrLn "Simulating CommitEther > "   
            putStrLn "What is your address? > "
            address <- getLine
            putStrLn (show address ++ " Make Commit greater than " ++ show val ++ " >")
            money <- getLine
            let moneyIn = (read money :: Ether) 
            let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st
            putStrLn ("Contract State after CommitEther: ") 
            prettyPrint no ns
            loop nc nco ns 
        (CommitEther (Max val) c1) -> do
            wait <- getLine
            putStrLn "Simulating CommitEther > "   
            putStrLn "What is your address? > "
            address <- getLine
            putStrLn (show address ++ " Make Commit less than " ++ show val ++ " >")
            money <- getLine
            let moneyIn = (read money :: Ether) 
            let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st
            putStrLn ("Contract State after CommitEther: ") 
            prettyPrint no ns
            loop nc nco ns 
        (CommitEther (Equal val) c1) -> do
            wait <- getLine
            putStrLn "Simulating CommitEther > "   
            putStrLn "What is your address? > "
            address <- getLine
            putStrLn (show address ++ " Commit " ++ show val ++ " >")
            money <- getLine
            let moneyIn = (read money :: Ether) 
            let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st
            putStrLn ("Contract State after CommitEther: ") 
            prettyPrint no ns
            loop nc nco ns 
        (CommitEther (NoLimit) c1) -> do
            wait <- getLine
            putStrLn "Simulating CommitEther > "   
            putStrLn "What is your address? (Provide any string) > "
            address <- getLine
            putStrLn (show address ++ " Commit Any Amount >")
            money <- getLine
            let moneyIn = (read money :: Ether) 
            let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st 
            putStrLn ("Contract State after CommitEther: ")
            prettyPrint no ns
            loop nc nco ns 
        (CommitEther (HigherThan str) c1) -> do
            wait <- getLine
            putStrLn "Simulating CommitEther > "   
            putStrLn "What is your address? (Provide any string) > "
            address <- getLine
            putStrLn (show address ++ " Commit Any Amount >")
            money <- getLine
            let moneyIn = (read money :: Ether) 
            let (nc, no, ns, nco) = run c (CashInp address moneyIn) pst st 
            putStrLn ("Contract State after CommitEther: ")
            prettyPrint no ns
            loop nc nco ns 

        (Send (Winner All) c1) -> do
            wait <- getLine
            putStrLn "Simulating Send > "
            putStrLn "Enter decision > "
            address <- getLine
            let dec = (read address :: Int)
            contractMember dec c pst st
            let (nc, no, ns, nco) = run c (Decision dec) pst st 
            putStrLn ("Contract State after Send")
            prettyPrint no ns
            loop nc nco ns

        (Send (Address All) c1) -> do
            wait <- getLine
            putStrLn "Simulating Send > "
            putStrLn "Choose Commit> "
            address <- getLine
            let dec = (read address :: Int)
            contractMember dec c pst st
            let (nc, no, ns, nco) = run c (WithdrawEther (Decision dec) (Amount 0)) pst st 
            putStrLn ("Contract State after Send")
            prettyPrint no ns
            loop nc nco ns

        (Send (ToBeneficiary (AmountIn str)) c1) -> do
            wait <- getLine
            putStrLn "Simulating Send > "
            let (nc, no, ns, nco) = run c (Empty) pst st 
            putStrLn ("Contract State after Send")
            prettyPrint no ns
            loop nc nco ns
        (Send (Random All) c1) -> do
            wait <- getLine
            putStrLn "Simulating Send > "
            let (nc, no, ns, nco) = run c (Empty) pst st 
            prettyPrint no ns
            loop nc nco ns

        (Withdraw c1) -> do 
            wait <- getLine
            putStrLn "Simulating Withdraw > "
            putStrLn "Choose Commit >"
            address <- getLine
            let wal = (read address :: Int)
            contractMember wal c pst st
            putStrLn "Withdraw Amount >"
            amount <- getLine
            let amnt = (read amount :: Ether)
            let (nc, no, ns, nco) = run c (WithdrawEther (Decision wal) (Amount amnt)) pst st
            putStrLn ("Contract State after Withdraw: ")
            prettyPrint no ns
            loop nc nco ns

        (Set (ContractOwner) c1) -> do
            wait <- getLine
            putStrLn "Simulating Set ContractOwner >"
            putStrLn "What is your address? (Provide any string) > "
            address <- getLine
            let (nc, no, ns, nco) = run c (SetOwner address) pst st
            putStrLn ("Contract State after Set ContractOwner: ")
            prettyPrint no ns
            loop nc nco ns
        (Set (Beneficiary) c1) -> do
            wait <- getLine
            putStrLn "Simulating Set Beneficiary >"
            putStrLn "What is the Beneficiary address? (Provide any string) > "
            address <- getLine
            let (nc, no, ns, nco) = run c (SetBeneficary address) pst st
            putStrLn ("Contract State after Set Beneficiary: ")
            prettyPrint no ns
            loop nc nco ns
        (Set (TimeLimit) c1) -> do
            wait <- getLine
            putStrLn "Simulating Set Timelimit >"
            putStrLn "What is the contract Timelimit (Provide time in days) > "
            days <- getLine
            let limit = (read days :: Int)
            let (nc, no, ns, nco) = run c (SetTimeLimit limit) pst st
            putStrLn ("Contract State after Set Timelimit: ")
            prettyPrint no ns
            loop nc nco ns
        (Until (People p) c1) -> do
            wait <- getLine
            putStrLn "Simulating Until >"
            putStrLn ("Function can be called until people = " ++ show p)
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns
        (Until (TimesUp) c1) -> do
            wait <- getLine
            putStrLn "Simulating Until >"
            putStrLn ("Function can be called until time is up")
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns
        (Unless (AlreadyJoined) c1) -> do 
            wait <- getLine
            putStrLn "Simulating Unless >"
            putStrLn "User can join contract unless they have already joined"
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns
        (When (People p) c1) -> do 
            wait <- getLine
            putStrLn "Simulating When >"
            putStrLn ("When there are " ++ show p ++ "people the next action can happen")
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns
        (When (TimesUp) c1) -> do 
            wait <- getLine
            putStrLn "Simulating When >"
            putStrLn ("When time is up, the next action can happen")
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns
        (Function str c1) -> do
            wait <- getLine
            putStrLn "Simulating Function >"
            putStrLn ("You have created Function called: " ++ str)
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns

        (Constructor c1) -> do
            wait <- getLine
            putStrLn "Simulating Constructor >"
            putStrLn "You have created a Constructor"
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns
        (AddTo str c1) -> do
            putStrLn ("Added to " ++ str)
            let (nc, no, ns, nco) = run c (Empty) pst st
            loop nc nco ns

        (End) -> putStrLn ("Contract simulation finished")

        c -> do 
            let (nc, no, ns, nco) = run c (Empty) pst st 
            loop nc nco ns 

prettyPrint :: OP -> ContractState -> IO ()
prettyPrint o const = do 
                      putStrLn ("Output: " ++ (show (niceOutput o))) 
                      putStrLn ("Commits: " ++ getCommitOutput const)
                      putStrLn ("Withdrawls: " ++ getSendOutput const)
                      putStrLn ("Contract Balance: " ++ show (etherBalance const))
                      putStrLn ("Owner: " ++ show (owner const))

getCommitAction :: Int -> ContractState -> (Int, Action)
getCommitAction n const = (n, ((commits const) ! n))

getWithdrawAction :: Int -> ContractState -> (Int, Action)
getWithdrawAction n const = (n, ((withdrawls const) ! n))


getCommitOutput :: ContractState -> String
getCommitOutput const = getCommitActionHelper (sizeCommits const) const

getSendOutput :: ContractState -> String
getSendOutput const = getWithdrawActionHelper (withdrawSize const) const

getWithdrawActionHelper :: Int -> ContractState -> String 
getWithdrawActionHelper x const 
    | (x == 0 ) = ""
    | otherwise = (show (getWithdrawAction (x) const)) ++ "," ++ (getWithdrawActionHelper (x-1) const)

getCommitActionHelper :: Int -> ContractState -> String 
getCommitActionHelper x const 
    | (x == 0 ) = ""
    | (x == 1) = (show (getCommitAction (x) const))
    | otherwise = (show (getCommitAction (x) const)) ++ "," ++ (getCommitActionHelper (x-1) const)

contractMember :: Int -> Contract -> ParamState -> ContractState -> IO ()
contractMember i c pst s
    | not (Map.member i (commits s)) = do 
        putStrLn "Wallet not a member of Contract"
        loop c pst s
    | otherwise = do 
        putStrLn ("Wallet " ++ getOneAddress (findAtIndex [i] s) ++ " chosen")

niceOutput :: OP -> Output
niceOutput [x] = x
