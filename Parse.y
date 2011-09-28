{
module Parse ( parseMoody, Statement(..) ) where
import Lexer

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
%token id { TId _ }
%token colon { TColon }
%token dname { TDName }
%token dtoken { TDToken }
%token dtokentype { TDTokenType }
%token code { TCode _ }

Grammar    : Intro Statements         { \a b -> a ++ (reverse b) }

Statements : Statements Statement     { \xs x -> x:xs }
Statements :                          { [] }
           
Statement  : Directive                { \x -> x }
Statement  : Production               { \x -> x }
           
Production : id colon Body code       { \(TId id) _ body (TCode c) -> Prod id (reverse body) c }

Body       : Body id                  { \xs (TId id) -> id:xs }
Body       :                          { [] }
           
Directive  : dname id                 { \_ (TId id) -> DName id }
Directive  : dtokentype code          { \_ (TCode c) -> DTokenType c }
Directive  : dtoken id code           { \_ (TId id) (TCode c) -> DToken id c }
           
Intro      : code                     { \(TCode c) -> [IntroCode c] }
Intro      :                          { [] }
