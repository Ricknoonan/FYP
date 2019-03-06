module HelloWorld where

import Contract
import ContractClass
import Prelude hiding (until, interact, return) 


setOwner :: Contract
setOwner = 
    (constructor (initiate (sayHello)))


sayHello :: Contract 
sayHello = 
    (function "sayHello" (return "HelloWorld" End))