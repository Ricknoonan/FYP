module CrowdFunding where

import Contract
import ContractClass
import Prelude hiding (until, not)

crowdFunding :: Contract
crowdFunding = setUpCrowdFunder

setUpCrowdFunder :: Contract
setUpCrowdFunder =
    (constructor (initiate (set (TimeLimit) (set (TotalAmount) donate))))

donate :: Contract
donate = 
    (function "donate" (until (TotalReached) (until (TimesUp) (commitEther (Min 0) withdrawOwner))))

withdrawOwner :: Contract
withdrawOwner = 
    (function "withdrawOwner" (allow (OnlyOwner) (when (TimesUp) (when (TotalReached) (send (Owner(All)) withdraw)))))

withdraw :: Contract
withdraw = 
    (function "withdraw" (when (TimesUp) (not (when (TotalReached) (send (Person (Withdraw)) End)))))