module Betting where

import Contract
import ContractClass
import Prelude hiding (until)

bettingContract :: Contract
bettingContract 
	      = 	(cashIn 20 1 
	                (cashIn 30 2
	                    (until (Date (2018,12,13))  
	                        (when (Date (2018,12,14))
	                            (pay 1 2 40 End)	                        
	                        End)	                    
	                    End)
	                End)                               	
	            End)
	        

