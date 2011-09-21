{
mudule Calc where 
}
%name calc

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
F : num { \TNum a -> a }
