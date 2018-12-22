module Lottery where

import Contract
import ContractClass


lotteryContract :: Contract
lotteryContract 
        = until (Date (2018,12,13))
          (buyInCycle 20 





