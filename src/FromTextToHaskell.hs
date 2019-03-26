module ToHaskell where

import Contract 
import ContractClass

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text


{--
test :: IO ()
test = do 
	contractText <- readFile "sampleContract.txt"
	let file = contractText
	concatMap (\y -> if y == 'when' then (read y :: When) else [y]) file
	--putStrLn (show file)
--}

test = do 
	ls <- fmap Text.words (Text.readFile "sampleContract.txt")
	let result = toHaskell ls 
	putStrLn (show result)

toHaskell :: [String] -> Contract 
toHaskell ("when":xs) = (when (toHaskell xs) End)
toHaskell ("People": "10" : xs) = (People 10 (toHaskell xs))
 