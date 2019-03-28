module Lottery where

import Contract
import ContractClass
import Prelude hiding (until)

join :: Contract  
join = 
    (function "join" (cashIn (Equal 1) (until (People 10) (unless (AlreadyJoined) End))))

selectWinner :: Contract 
selectWinner = 
    (function "selectWinner" (when (People 10) (send (Random All) join)))

lottery :: Contract  
lottery = selectWinner