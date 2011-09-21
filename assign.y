%name assign
%token id { Id }
%token star { Star }
%token eq { Eq }
%tokentype { Z }

S : L eq R { }
S : R { }
L : star R { }
L : id { }
R : L { }

