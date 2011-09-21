module Grammar (GrammarDef(..), SymId, GrammarSym(..), Production(..), TokenInfo, 
	buildGrammar, nullSymbol, isNt, getNt, symId, isTerm,
	prodHead, prodBody, prodCode, isProdStart) where
	
import Parse
import Utils
import Data.List
import Data.Maybe
import Data.Array
import Data.Function
import Text.Show.Functions

data GrammarDef = GrammarDef {
	start :: SymId,
	prods :: [Production],
	tokens :: [TokenInfo],
	name :: String,
	tokentype :: String,
	intro :: String,
	
	syms :: [SymId],
	nts :: [SymId],
	prodByNo :: Int -> Production,
	prodsByNt :: SymId -> [Int]
	} deriving Show
	
type SymId = String
data GrammarSym = Term !SymId | NT !SymId | Eps | Dummy deriving (Show,Eq,Ord)
type Production = (SymId, [GrammarSym], String, Bool) -- nt, body, code, isStart
type TokenInfo = (SymId, String) -- name, code

buildGrammar :: [Statement] -> GrammarDef
buildGrammar stmts = let
		tokens = findTokens stmts
		prods' = findProds (fst.unzip$ tokens) stmts
		start = prodHead.head$ prods'
		prods = ("_start", [NT start], "", True) : prods' -- augment
		name = head$ mapMaybe getName stmts 
			where { getName (DName n) = Just n; getName _ = Nothing }
		tokentype = head$ mapMaybe getTType stmts 
			where { getTType (DTokenType n) = Just n; getTType _ = Nothing }
		intro = intercalate "\n" . mapMaybe getCode $ stmts
			where { getCode (IntroCode s) = Just s; getCode _ = Nothing }
		
		prodArr = listArray (0, length prods-1) prods
		prodByNo = (prodArr!)
		prodsByNt = fromMaybe [] . flip lookup (classifyPairs z)
			where z = zip [0..] (map prodHead prods)
		nts = nub $ map prodHead prods
		syms = nts ++ (map fst tokens)
	in
		(GrammarDef start prods tokens name tokentype intro syms nts prodByNo prodsByNt)
	
findTokens :: [Statement] -> [TokenInfo]
findTokens = mapMaybe conv where
		conv (DToken name match) = Just (name, match)
		conv _ = Nothing

findProds :: [String] -> [Statement] -> [Production]
findProds tokens = mapMaybe conv where
		conv (Prod nt body code) = Just (nt, map convSym body, code, False)
		conv _ = Nothing
		convSym s | s `elem` tokens = Term s
		convSym s = NT s
		
nullGrammar = GrammarDef "" [] [] "" "" ""
nullSymbol = ""

prodHead :: Production -> SymId
prodHead (h, _, _, _) = h

prodBody :: Production -> [GrammarSym]
prodBody (_, b, _, _) = b
isProdStart (_, _, _, s) = s
prodCode (_, _, c, _) = c

isNt x = case x of { (NT _) -> True; _ -> False }
isTerm x = case x of { (Term _) -> True; _ -> False }
getNt x = case x of { (NT s) -> Just s; _ -> Nothing }
symId (NT n) = n
symId (Term n) = n

