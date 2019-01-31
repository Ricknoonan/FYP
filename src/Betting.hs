module Betting where

import Contract
import ContractClass
import Prelude hiding (until)

bettingContract :: Contract
bettingContract 
              = (until (OrOb (Date (2018,12,13)) (Amount 100))
                    (cashIn 20 1 
                    (cashIn 20 2                     
                        (when (Ob (Amount 40))
                            (pay 1 2 40 End)	                        
	                        End)	                    
	                    End)
	                End)                               	
	            End)





	        

