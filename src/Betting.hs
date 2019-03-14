module Betting where

import Contract
import ContractClass
import Prelude hiding (until)

bettingContract :: Contract
bettingContract = 
    (constructor(initiate (finalize)))

join :: Contract
join = 
    (function "join" (until (People 2) (cashIn (Equal 5) End ))) 
   
finalize :: Contract
finalize = 
    (function "finalize" (when (Amount 10) (send (Person All) (join))))

--(constructor (initiate (funtion "finalize" (when (Amount 10) (send (Winner All) (funtion "join" (until (People 2) (cashIn (Equal 5) End ))))))))






