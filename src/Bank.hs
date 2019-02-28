module Bank where

import Contract
import ContractClass
import Prelude hiding (until, interact) 

bank :: Contract
bank = (constructor (initiate (deposit)))

cashOut :: Contract
cashOut = 
	(function "cashOut" (send Person (Withdraw)) End)

deposit :: Contract 
deposit = 
	(function "deposit" (cashIn (NoLimit) (cashOut)))

 
--(constructor (initiate (function "deposit" (cashIn (NoLimit) (function "cashOut" (send Person (Withdraw)) End))))