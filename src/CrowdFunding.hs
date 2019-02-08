module CrowdFunding where

import Contract
import ContractClass
import Prelude hiding (until)

crowdfundingContract :: Contract 
crowdfundingContract 
              = (until (Ob (Date (2018,12,13)))
                    (cashInUnlimited address (people 3)
                        (when (Ob(Amount 50))
                        	(pay (Ob (Winner address)) End)                         	
                            (cashBackAll End))
                    End)
                End)