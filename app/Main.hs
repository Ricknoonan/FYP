module Main (
        main
    ) where

import Contract
import ContractClass
import Prelude hiding (until)
import Data.Char(digitToInt)

main :: IO ()
main = do 
    let input = evalAll(bettingContract)
    putStrLn(prettyPrint input)

prettyPrint :: (Contract,OP,State) -> String
prettyPrint (con, op, st) = (show con ++ show op ++ show st)

bettingContract :: Contract
bettingContract 
        =   (until (OrOb (Date (2018,12,13)) (Amount 100))
                (cashIn 20 1
                    (cashIn 20 2
                        (when (Ob (Date (2018,12,14)))
                            (pay 1 2 40 End)                            
                        End)                        
                    End)
                End)
            End)    

{--
crowdFunder :: Contract 
crowdFunder 
             = (until (OrOb (Date (2018,12,13)) Amount 100) 
                    (cashInUnlimited input 1 
                        (cashInUnlimited input 2 
                            (cashInUnlimited input 3
                                (cashInUnlimited input 4 )   
--}              
{--
type ErrorWithIO = ExceptT String IO

foo :: String -> ErrorWithIO String
foo "Yes" = do liftIO $ putStrLn "Paul!"
                return "OK!"
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


look :: InputT IO ()
loop = do 
    line <- getInputLine "Bet amount: "
    case line of
        Nothing -> return ()
        Just input -> do 
            case unsafePerformIO $ runExceptT $ 
                End)

--}


