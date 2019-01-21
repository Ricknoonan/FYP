import Contract
import ContractClass

zcb :: Date -> Double -> Transfer -> Contract
zcb t x k = time (at t) (scale (x) (one k))

singleton :: Transfer -> Contract
singleton cur = one cur

scaleContract :: Double -> Double -> Transfer -> Contract
scaleContract x y t = scale x (scale y (one t))

--Outputs primitive contract

c1 :: Contract
c1 = zcb 100 (Date 10) (Currency EUR)

c2 :: Contract
c2 = zcb 120 (Date 12) (Currency EUR)

c3 :: Contract 
c3 = singleton (Currency EUR)

c4 :: Contract 
c4 = scaleContract 10 20 (Currency EUR)

-- Outputs contract with calculations with more meaningful data types

output0 :: ReadableContract
output0 = evalR(zero)

output1 :: ReadableContract
output1 = evalR(c3)

output2 :: ReadableContract
output2 = evalR (c4)

output3 :: ReadableContract 
output3 = evalR (c1 `And` c2)

output4 :: ReadableContract
output4 = evalR (c1 `Or` c2)