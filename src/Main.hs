module Main where

import Contract
import ContractClass
import Betting

main :: IO ()
main = do
    let main = evalState (c1 initialstate)
    print main