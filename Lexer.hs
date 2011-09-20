module Lexer (Token(..), lexMoody) where
import Data.Char

data Token
	= TId String
	| TColon
	| TDName
	| TDToken
	| TDTokenType
	| TCode String
	deriving Show

lexMoody :: String -> [Token]
lexMoody x = case x of
	[] -> []
	(c:cs) | isSpace c -> lexMoody cs
	(c:cs) | isAlpha c -> TId name : lexMoody rest
		where (name, rest) = span isAlpha (c:cs)
	(':':cs) -> TColon : lexMoody cs
	('%':cs) -> let (name, rest) = span isAlpha cs in case name of
		"name" -> TDName : lexMoody rest
		"tokentype" -> TDTokenType : lexMoody rest
		"token" -> TDToken : lexMoody rest
	('{':cs) -> TCode code : lexMoody rest
		where (code, rest) = spanCode 0 "" cs
	
spanCode :: Integer -> String -> String -> (String, String)
spanCode 0 code ('}':cs) = (code, cs)
spanCode i code ('{':cs) = spanCode (i+1) (code++"{") cs
spanCode i code ('}':cs) = spanCode (i-1) (code++"}") cs
spanCode i code (c:cs) = spanCode i (code++[c]) cs
