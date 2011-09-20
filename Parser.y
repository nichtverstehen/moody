{
module Parser ( parseMoody, Statement(..) ) where
import Lexer

happyError x = error $ "parse error " ++ (show x)
data Statement
	= Prod String [String] String
	| IntroCode String
	| DName String
	| DTokenType String
	| DToken String String
	deriving Show
}

%name parseMoody
%tokentype { Token }
%token
	id { TId $$ }
	colon { TColon }
	dname { TDName }
	dtoken { TDToken }
	dtokentype { TDTokenType }
	code { TCode $$ }

%%

Grammar    : Intro Statements         { $1 ++ (reverse $2) }

Statements : Statements Statement     { $2:$1 }
           |                          { [] }
           
Statement  : Directive                { $1 }
           | Production               { $1 }
           
Production : id colon Body code       { Prod $1 (reverse $3) $4 }

Body       : Body id                  { $2:$1 }
           |                          { [] }
           
Directive  : dname id                 { DName $2 }
           | dtokentype code          { DTokenType $2 }
           | dtoken id code           { DToken $2 $3 }
           
Intro      : code                     { [IntroCode $1] }
           |                          { [] }
