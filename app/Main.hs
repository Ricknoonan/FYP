module Main (
        main
    ) where
    
import Contract
import ContractClass
import Prelude hiding (until)

main :: IO ()
main = do 
    let input = evalAll(bettingContract)
    putStrLn(prettyPrint input)

prettyPrint :: (Contract,OP,State) -> String
prettyPrint (con, op, st) = ("Contract has finished! " ++ " This is how the contract was evaluated: " ++ show op ++ " And the final state of the contract: " ++ show st)

bettingContract :: Contract
bettingContract 
          = until (Date (2018,12,13)) 
                (cashIn 20 1 
                    (cashIn 30 2 
                        (time (Date (2018,12,14))
                            (pay 1 2 40 End)                            
                        End)
                    End)                            
                End)
            End


