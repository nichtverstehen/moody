module Moody where
import Parse
import Lexer
import LALR
import Grammar
import CodeGen
import System (getArgs)
import Data.Maybe
import Data.List

run src = let 
	gr = (buildGrammar.parseMoody.lexMoody) src
	first = lrFirst gr
	coll = makeCollection gr
	sp = spontaneousLookaheads gr first coll
	pr = propagatedLookaheads  gr first coll
	la = computeLookaheads pr sp
	r = enrichItems la coll
	action = makeActionTable gr r
	goto = makeGotoTable gr r
	code = generateCode gr goto action
	in code

main = do
	srcName <- getArgs >>= return.head
	dstName <- return (reverse (fromMaybe (reverse srcName) $ stripPrefix "y." (reverse srcName)) ++ ".hs")
	src <- readFile srcName
	dst <- return$ run src
	writeFile dstName dst
		
