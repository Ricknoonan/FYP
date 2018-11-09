--Types: 
--Person as a their key 
--Blocknumber
--Random int that represents values of something

--Datatypes:
--Observable type made of key and blocknumber
--OS type represents values available at a step 

--Identifies:
-- Commitments, choices and payments are identified by identifiers

--Datatypes: 
-- A cash commitment made by a person 
-- A chah redemption made by a person

-- Input Datatype:
-- This is made up of sets 

-- 1. Give some input to the contract i.e make a payment
-- 2. Give the current state
-- 3. Give a contract
-- 4. Give observable 
-- 5. Return some action that is acted on the blockchain 

--contractToReadable :: Double -> Double -> Transfer -> ReadableContract 
--contractToReadable x y transfer = BetContract ((show transfer)++(show (multiply (Scale x (Scale y (One transfer))))) ++ " bet at " ++ (show y))