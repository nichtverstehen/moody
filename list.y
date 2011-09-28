{
import Data.Char
data ListToken = ListId String | ListComma deriving Show

lexList [] = []
lexList (' ':cs) = lexList cs
lexList (',':cs) = ListComma : lexList cs
lexList x = ListId id : lexList rest
	where (id, rest) = span isAlpha x
}

%name parseList
%tokentype { ListToken }
%token , { ListComma }
%token id { ListId _ }

List : id List'    { \(ListId id) rest -> id : rest }
List' :            { [] }
List' : , List     { \_ rest -> rest }
