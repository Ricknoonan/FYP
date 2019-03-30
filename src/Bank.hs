module Bank where

import Contract
import ContractClass
import Prelude hiding (until, interact) 

bank :: Contract 
bank = (constructor (set (ContractOwner) deposit)) 
            
deposit :: Contract 
deposit = 
    (function "deposit" (commitEther (NoLimit) (addTo "balance" cashOut )))

cashOut :: Contract
cashOut = 
    (function "cashOut" (withdraw End))