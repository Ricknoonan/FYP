module Main (
        main
    ) where

import Contract
import ContractClass
import Prelude hiding (until)
import Betting 
import CrowdFunding
import System.IO.Unsafe (unsafePerformIO)



main :: IO ()
main = do 
    loop bettingContract emptyOb emptyState
    --loop crowdfundingContract emptyOb emptyState

loop :: Contract -> ControlObs -> State -> IO ()
loop c co s = do
    case c of 
        (CashIn val addr people c1 c2) -> do 
                                putStrLn "What is your wallet address? > "
                                address <- getLine
                                putStrLn (show address ++ " Commit " ++ show val ++ " >")
                                input <- getLine 
                                let (nc, no, ns, nco) = run (CashIn val address people c1 c2) input co s 
                                putStrLn (show no ++ show ns)
                                loop nc nco ns 

        (Pay addr c1) -> do 
                                putStrLn "Enter decision > "
                                input <- getLine 
                                let (nc, no, ns, nco) = run c input co s
                                putStrLn (show no)
                                loop nc nco ns

        (CashInUnlimited addr people c1 c2) -> do
                                putStrLn "What is your wallet address? > "
                                addr <- getLine
                                putStrLn "Enter Commit > "
                                input <- getLine 
                                let (nc, no, ns, nco) = run (CashInUnlimited addr people c1 c2) input co s 
                                putStrLn (show no ++ show ns)
                                loop nc nco ns



        (End) -> putStrLn ("Contract finished")

        c -> do 
            let (nc, no, ns, nco) = run c "Nothing" co s 
            loop nc nco ns 

{--
foo :: String -> ExceptT String IO
foo "Yes" = do liftIO $ putStrLn "Paul!"
foo _ = throwError "ERROR!"


runRepl :: IO ()
runRepl = runInputT defaultSettings $ loop

loop :: InputT IO ()
loop = do
    line <- getInputLine "> "
    case line of
        Nothing -> return ()
        Just input -> do 
          liftIO (putStrLn "asd")
          x <- liftIO (runExceptT $ foo input)
          case x of
            Left err  -> outputStrLn err
            Right res -> outputStrLn (show res)
          loop

main :: IO ()
main = runRepl >> putStrLn "Goodbye!"

--}