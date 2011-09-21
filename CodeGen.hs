module CodeGen where
import LALR
import Grammar
import Text.Printf
import Data.List
import Data.Array
import Data.Char

generateStateCode :: GrammarDef -> GotoState -> ActionState -> Int -> String
generateStateCode gr goto action no = let
	fnText = printf "state_" ++ (show no) ++" inp@(token:tokens) stack = act x where\n" ++
		"    x = case token of\n" ++ actionCodes ++ "\n" ++
		"    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)\n" ++
		"    act e@(End_, _, _, _) = e\n"++
		"    act (t, _, rest, red) = act r where\n" ++
		"      r = case t of\n" ++ gotoCode ++ "\n" ++
		"        _ -> error \"parse\"\n\n"
	
	actionCodes :: String
	actionCodes = intercalate "\n" $ map genTermCode action
	genTermCode (sym, act) = "        (" ++ (getTermPattern sym) ++ ") -> " ++ (genTermAction act)
	genTermAction (Shift st) = "state_" ++ (show st) ++ " tokens ((dataToken tv):stack)"
	genTermAction (Reduce nt) = "("++ prHead ++ ", " ++ (show prLen) ++", inp, calcValue_" ++ (show nt) ++ " stack)"
		where prod = prodByNo gr nt; prHead = prodHead prod; prLen = (length.prodBody) prod
	genTermAction (Accept) = "(End_, 0, tokens, head stack)"
	getTermPattern (Term n) = "Terminal_ tv@(" ++ (tokenCode gr n) ++ ")"
	getTermPattern Eps = "End_"
	
	gotoCode :: String
	gotoCode = (intercalate "\n" $ map genGotoCode goto)
	genGotoCode (n, st) = "        " ++ n ++ " -> state_" ++ (show st) ++ " rest (red:stack)"
		
	in fnText

generateCode :: GrammarDef -> GotoTable -> ActionTable -> String
generateCode gr goto action = let
	stateBounds = bounds goto
	introCode = (intro gr) ++ "\n"
	
	wrapper = (name gr) ++ " tokens = case (state_0 (map Terminal_ tokens ++ [End_]) []) of\n" ++
		"    (_, _, _, red) -> undata_" ++ (start gr) ++" red \n\n"
	ntsConstr = intercalate " | " (tail$ nts gr) -- tail to skin _start
	head = "data Token_ = End_ | Terminal_ " ++ (tokentype gr) ++" | " ++ ntsConstr ++ " deriving Show\n\n"
	
	ntsCount = (length$ nts gr) - 1
	ntsWithNo = [0..] `zip` (tail$ nts gr)
	tokenBuilder = "dataToken t = (Just t, " ++ (nothings ntsCount) ++ ") \n"
	dataBuilders = concat $ map genDataBuilder ntsWithNo
	genDataBuilder (no, name) = "data_" ++ name ++ " a = (Nothing, " ++ (nothingsIntr ntsCount no) ++ ")\n"
	nothingsIntr n e = (intercalate ", " . filter (not.null)) [(nothings e), "Just a", (nothings (n-e-1))]
	nothings n = intercalate ", " $ replicate n "Nothing"
	
	prods' = tail $ (prods gr) `zip` [0..]
	valueCalcs = concat $ map genValueCalc prods'
	genValueCalc ((nt, body, code, _), no) = "calcPred_" ++ (show no) ++ " = " ++ code ++ "\n"
	
	dataGetters = concat$ map genDataGetter ((-1, "Token_") : ntsWithNo)
	genDataGetter (no, name) = "undata_" ++ name ++ " ("++ (underscores (no+1) (ntsCount+1)) ++ ") = x\n"
	underscores n w = intercalate ", " $ (replicate n "_") ++ ["Just x"] ++ (replicate (w-n-1) "_")
	
	productionCalcs = concat $ map genProductionCalc prods'
	genProductionCalc ((nt, body, code, _), no) = "calcValue_" ++ (show no) ++ 
		" (" ++ argt ++ "xs) = data_" ++ (nt) ++ " $ calcPred_" ++ (show no) ++ " " ++ unpack ++ "\n" where
			letters = take (length body) ['a'..]
			argt = concatMap (\x -> x:":") (reverse letters)
			unpack = concatMap genArgPat (body `zip` ['a'..])
			genArgPat ((Term n), l) = "(undata_Token_ " ++ [l] ++ ") "
			genArgPat ((NT n), l) = "(undata_" ++ n ++ " " ++ [l] ++ ") "
	
	stateFns = map (\i -> generateStateCode gr (goto!i) (action!i) i) [fst stateBounds .. snd stateBounds]
	in introCode ++ wrapper ++ tokenBuilder ++ dataBuilders ++ dataGetters ++ valueCalcs ++ productionCalcs ++ "\n\n" ++ 
		head ++ (intercalate "\n\n" stateFns)
