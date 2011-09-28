
module Parse ( parseMoody, Statement(..) ) where
import Lexer

data Statement
	= Prod String [String] String
	| IntroCode String
	| DName String
	| DTokenType String
	| DToken String String
	deriving Show

parseMoody tokens = case (state_0 (map Terminal_ tokens ++ [End_]) []) of
    (_, _, _, red) -> undata_Grammar red 

dataToken t = (Just t, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) 
data_Grammar a = (Nothing, Just a, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
data_Statements a = (Nothing, Nothing, Just a, Nothing, Nothing, Nothing, Nothing, Nothing)
data_Statement a = (Nothing, Nothing, Nothing, Just a, Nothing, Nothing, Nothing, Nothing)
data_Production a = (Nothing, Nothing, Nothing, Nothing, Just a, Nothing, Nothing, Nothing)
data_Body a = (Nothing, Nothing, Nothing, Nothing, Nothing, Just a, Nothing, Nothing)
data_Directive a = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just a, Nothing)
data_Intro a = (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just a)
undata_Token_ (Just x, _, _, _, _, _, _, _) = x
undata_Grammar (_, Just x, _, _, _, _, _, _) = x
undata_Statements (_, _, Just x, _, _, _, _, _) = x
undata_Statement (_, _, _, Just x, _, _, _, _) = x
undata_Production (_, _, _, _, Just x, _, _, _) = x
undata_Body (_, _, _, _, _, Just x, _, _) = x
undata_Directive (_, _, _, _, _, _, Just x, _) = x
undata_Intro (_, _, _, _, _, _, _, Just x) = x
calcPred_1 =  \a b -> a ++ (reverse b) 
calcPred_2 =  \xs x -> x:xs 
calcPred_3 =  [] 
calcPred_4 =  \x -> x 
calcPred_5 =  \x -> x 
calcPred_6 =  \(TId id) _ body (TCode c) -> Prod id (reverse body) c 
calcPred_7 =  \xs (TId id) -> id:xs 
calcPred_8 =  [] 
calcPred_9 =  \_ (TId id) -> DName id 
calcPred_10 =  \_ (TCode c) -> DTokenType c 
calcPred_11 =  \_ (TId id) (TCode c) -> DToken id c 
calcPred_12 =  \(TCode c) -> [IntroCode c] 
calcPred_13 =  [] 
calcValue_1 (b:a:xs) = data_Grammar $ calcPred_1 (undata_Intro a) (undata_Statements b) 
calcValue_2 (b:a:xs) = data_Statements $ calcPred_2 (undata_Statements a) (undata_Statement b) 
calcValue_3 (xs) = data_Statements $ calcPred_3 
calcValue_4 (a:xs) = data_Statement $ calcPred_4 (undata_Directive a) 
calcValue_5 (a:xs) = data_Statement $ calcPred_5 (undata_Production a) 
calcValue_6 (d:c:b:a:xs) = data_Production $ calcPred_6 (undata_Token_ a) (undata_Token_ b) (undata_Body c) (undata_Token_ d) 
calcValue_7 (b:a:xs) = data_Body $ calcPred_7 (undata_Body a) (undata_Token_ b) 
calcValue_8 (xs) = data_Body $ calcPred_8 
calcValue_9 (b:a:xs) = data_Directive $ calcPred_9 (undata_Token_ a) (undata_Token_ b) 
calcValue_10 (b:a:xs) = data_Directive $ calcPred_10 (undata_Token_ a) (undata_Token_ b) 
calcValue_11 (c:b:a:xs) = data_Directive $ calcPred_11 (undata_Token_ a) (undata_Token_ b) (undata_Token_ c) 
calcValue_12 (a:xs) = data_Intro $ calcPred_12 (undata_Token_ a) 
calcValue_13 (xs) = data_Intro $ calcPred_13 


data Token_ = End_ | Terminal_  Token  | Grammar | Statements | Statement | Production | Body | Directive | Intro deriving Show

state_0 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TCode _ )) -> state_1 tokens ((dataToken tv):stack)
        (Terminal_ tv@(TDName )) -> (Intro, 0, inp, calcValue_13 stack)
        (Terminal_ tv@(TDToken )) -> (Intro, 0, inp, calcValue_13 stack)
        (Terminal_ tv@(TDTokenType )) -> (Intro, 0, inp, calcValue_13 stack)
        (Terminal_ tv@(TId _ )) -> (Intro, 0, inp, calcValue_13 stack)
        (End_) -> (Intro, 0, inp, calcValue_13 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of
        Grammar -> state_3 rest (red:stack)
        Intro -> state_2 rest (red:stack)
        _ -> error "parse"



state_1 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Intro, 1, inp, calcValue_12 stack)
        (Terminal_ tv@(TDToken )) -> (Intro, 1, inp, calcValue_12 stack)
        (Terminal_ tv@(TDTokenType )) -> (Intro, 1, inp, calcValue_12 stack)
        (Terminal_ tv@(TId _ )) -> (Intro, 1, inp, calcValue_12 stack)
        (End_) -> (Intro, 1, inp, calcValue_12 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_2 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Statements, 0, inp, calcValue_3 stack)
        (Terminal_ tv@(TDToken )) -> (Statements, 0, inp, calcValue_3 stack)
        (Terminal_ tv@(TDTokenType )) -> (Statements, 0, inp, calcValue_3 stack)
        (Terminal_ tv@(TId _ )) -> (Statements, 0, inp, calcValue_3 stack)
        (End_) -> (Statements, 0, inp, calcValue_3 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of
        Statements -> state_4 rest (red:stack)
        _ -> error "parse"



state_3 inp@(token:tokens) stack = act x where
    x = case token of
        (End_) -> (End_, 0, tokens, head stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_4 inp@(token:tokens) stack = act x where
    x = case token of
        (End_) -> (Grammar, 2, inp, calcValue_1 stack)
        (Terminal_ tv@(TId _ )) -> state_8 tokens ((dataToken tv):stack)
        (Terminal_ tv@(TDName )) -> state_7 tokens ((dataToken tv):stack)
        (Terminal_ tv@(TDTokenType )) -> state_5 tokens ((dataToken tv):stack)
        (Terminal_ tv@(TDToken )) -> state_6 tokens ((dataToken tv):stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of
        Statement -> state_11 rest (red:stack)
        Production -> state_10 rest (red:stack)
        Directive -> state_9 rest (red:stack)
        _ -> error "parse"



state_5 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TCode _ )) -> state_12 tokens ((dataToken tv):stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_6 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TId _ )) -> state_13 tokens ((dataToken tv):stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_7 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TId _ )) -> state_14 tokens ((dataToken tv):stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_8 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TColon )) -> state_15 tokens ((dataToken tv):stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_9 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Statement, 1, inp, calcValue_4 stack)
        (Terminal_ tv@(TDToken )) -> (Statement, 1, inp, calcValue_4 stack)
        (Terminal_ tv@(TDTokenType )) -> (Statement, 1, inp, calcValue_4 stack)
        (Terminal_ tv@(TId _ )) -> (Statement, 1, inp, calcValue_4 stack)
        (End_) -> (Statement, 1, inp, calcValue_4 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_10 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Statement, 1, inp, calcValue_5 stack)
        (Terminal_ tv@(TDToken )) -> (Statement, 1, inp, calcValue_5 stack)
        (Terminal_ tv@(TDTokenType )) -> (Statement, 1, inp, calcValue_5 stack)
        (Terminal_ tv@(TId _ )) -> (Statement, 1, inp, calcValue_5 stack)
        (End_) -> (Statement, 1, inp, calcValue_5 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_11 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Statements, 2, inp, calcValue_2 stack)
        (Terminal_ tv@(TDToken )) -> (Statements, 2, inp, calcValue_2 stack)
        (Terminal_ tv@(TDTokenType )) -> (Statements, 2, inp, calcValue_2 stack)
        (Terminal_ tv@(TId _ )) -> (Statements, 2, inp, calcValue_2 stack)
        (End_) -> (Statements, 2, inp, calcValue_2 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_12 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Directive, 2, inp, calcValue_10 stack)
        (Terminal_ tv@(TDToken )) -> (Directive, 2, inp, calcValue_10 stack)
        (Terminal_ tv@(TDTokenType )) -> (Directive, 2, inp, calcValue_10 stack)
        (Terminal_ tv@(TId _ )) -> (Directive, 2, inp, calcValue_10 stack)
        (End_) -> (Directive, 2, inp, calcValue_10 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_13 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TCode _ )) -> state_16 tokens ((dataToken tv):stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_14 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Directive, 2, inp, calcValue_9 stack)
        (Terminal_ tv@(TDToken )) -> (Directive, 2, inp, calcValue_9 stack)
        (Terminal_ tv@(TDTokenType )) -> (Directive, 2, inp, calcValue_9 stack)
        (Terminal_ tv@(TId _ )) -> (Directive, 2, inp, calcValue_9 stack)
        (End_) -> (Directive, 2, inp, calcValue_9 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_15 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TCode _ )) -> (Body, 0, inp, calcValue_8 stack)
        (Terminal_ tv@(TId _ )) -> (Body, 0, inp, calcValue_8 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of
        Body -> state_17 rest (red:stack)
        _ -> error "parse"



state_16 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Directive, 3, inp, calcValue_11 stack)
        (Terminal_ tv@(TDToken )) -> (Directive, 3, inp, calcValue_11 stack)
        (Terminal_ tv@(TDTokenType )) -> (Directive, 3, inp, calcValue_11 stack)
        (Terminal_ tv@(TId _ )) -> (Directive, 3, inp, calcValue_11 stack)
        (End_) -> (Directive, 3, inp, calcValue_11 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_17 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TCode _ )) -> state_18 tokens ((dataToken tv):stack)
        (Terminal_ tv@(TId _ )) -> state_19 tokens ((dataToken tv):stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_18 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TDName )) -> (Production, 4, inp, calcValue_6 stack)
        (Terminal_ tv@(TDToken )) -> (Production, 4, inp, calcValue_6 stack)
        (Terminal_ tv@(TDTokenType )) -> (Production, 4, inp, calcValue_6 stack)
        (Terminal_ tv@(TId _ )) -> (Production, 4, inp, calcValue_6 stack)
        (End_) -> (Production, 4, inp, calcValue_6 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"



state_19 inp@(token:tokens) stack = act x where
    x = case token of
        (Terminal_ tv@(TCode _ )) -> (Body, 2, inp, calcValue_7 stack)
        (Terminal_ tv@(TId _ )) -> (Body, 2, inp, calcValue_7 stack)
    act (r, n, rest, red) | n > 0 = (r, n-1, rest, red)
    act e@(End_, _, _, _) = e
    act (t, _, rest, red) = act r where
      r = case t of

        _ -> error "parse"

