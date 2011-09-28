{
data M = S Int Int deriving Show
data SToken = A Int | B Int deriving Show
happyError x = error $ "parse error " ++ (show x)
}
%name simple
%tokentype { SToken }
%token
	a { A $$ }
	b { B $$ }
%%

S : A B { S $1 $2 }
A : a { $1 }
B : b { $1 }
