%name ccd
%token c { C }
%token d { D }
%tokentype { CDToken }

S : C C { }
C : c C { }
C : d { }
