{
import Data.Char
import System
data Token = TPlus | TMult | TLParen | TRParen | TNum Int deriving Show

lexCalc [] = []
lexCalc (' ':cs) = lexCalc cs
lexCalc ('+':cs) = TPlus : lexCalc cs
lexCalc ('*':cs) = TMult : lexCalc cs
lexCalc ('(':cs) = TLParen : lexCalc cs
lexCalc (')':cs) = TRParen : lexCalc cs
lexCalc (c:cs) | isDigit c = TNum (read num) : lexCalc cs
	where (num, rest) = span isDigit (c:cs)

main = do
	args <- getArgs
	let src = head args
	let tokens = lexCalc src
	let res = parseCalc tokens
	putStrLn (show res)

}
%name parseCalc

%tokentype { Token }
%token plus { TPlus }
%token mult { TMult }
%token lparen { TLParen }
%token rparen { TRParen }
%token num { TNum _ }

E : E plus T { \a _ b -> a + b }
E : T { \a -> a }
T : T mult F { \a _ b -> a * b }
T : F { \a -> a }
F : lparen E rparen { \_ a _ -> a }
F : num { \(TNum a) -> a }
