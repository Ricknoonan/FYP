module Betting where

import Contract
import ContractClass
import Prelude hiding (until)

bettingContract :: Contract
bettingContract 
              = (until (EitherOb (Date (2018,12,13)) (Amount 100))
                    (cashIn 20 address (people 5) 
                        (pay (Ob (Winner address)) End) 
	                End)
	            End)                               	
	        

