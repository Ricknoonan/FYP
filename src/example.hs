{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import Data.Time
import Data.Attoparsec.Char8
import Control.Applicative

import Contract 
import ContractClass 

parser :: Contract 
parser = do 
	char '('
	c1 <- contract
	case c1 of 
		'send' -> char '(' 
		          




