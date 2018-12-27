module Betting where

import Contract
import ContractClass
import Prelude hiding (until)

bettingContract :: Contract
bettingContract 
	      = until (Date (2018,12,13)) 
	            (cashIn 20 1 
	                (cashIn 20 2 
	                    (time (Date (2018,12,15))
	                        (pay 1 2 40 End)
	                    End)
	                End)                           	
	            End)
	        End

c1 = evalAll(bettingContract)
