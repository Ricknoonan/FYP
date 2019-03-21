module Bank where

import Contract
import ContractClass
import Prelude hiding (until, interact) 

bank :: Contract 
bank = (constructor (set (ContractOwner) (cashOut)))

cashOut :: Contract
cashOut = 
    (function "cashOut" (withdraw deposit))

deposit :: Contract 
deposit = 
    (function "deposit" (cashIn (NoLimit) (addTo "balance" End )))
