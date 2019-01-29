module Main (
        main
    ) where

import Contract
import ContractClass
import Prelude hiding (until)
import Betting 
import System.IO.Unsafe (unsafePerformIO)

{--
main :: Contract -> IO ()
main c = do 
    putStrLn "Enter Input: "
    input <- getLine
    case input of 
        Nothing -> return ()
        Just lineIn -> case lineIn of
            Nothing -> return ()
            Just inputted 
--}

main :: IO ()
main = do 
    control (bettingContract)

control :: Contract -> IO ()
control c = do 
    putStrLn (show c) 
    let res = loop c
    let (nc, no, ns) = evalAll c (unsafePerformIO (res))
    control nc 
    putStrLn ("Contract finished")

loop :: Contract -> IO Money
loop c = do 
    case c of 
        (Until obs c1 c2) -> return (0)
        (CashIn val person c1 c2)  -> do 
                                putStrLn "Enter Input: "
                                input <- getLine 
                                return (read input :: Money)
        {--
        (Pay person1 person2 val c1) -> do 
                                putStrLn "Enter decision: "
                                input <- getLine 
                                return (read input :: Person)
       --}

{--
    unsafePerformIO $ 
    let (c,o,s) = evalAll(bettingContract)
    putStrLn(prettyPrint input)

prettyPrint :: (Contract,OP,State) -> String
prettyPrint (con, op, st) = (show con ++ show op ++ show st)

type ErrorWithIO = ExceptT String IO


foo :: String -> ErrorWithIO String
foo "Yes" = do liftIO $ putStrLn "Paul!"
foo _ = throwError "ERROR!"


runRepl :: IO ()
runRepl = runInputT defaultSettings $ loop

loop :: InputT IO ()
loop = do
    line <- getInputLine "Bet amount: "
    case line of
        Nothing -> return ()
        Just input -> do return $ putStrLn "asd"
                         case unsafePerformIO $ runExceptT $ foo input of
                             Left err -> outputStrLn err >> loop
                             Right res -> do
                                 x <- outputStrLn . show $ res
                                 loop

main :: IO ()
main = runRepl >> putStrLn "Goodbye!"

--}