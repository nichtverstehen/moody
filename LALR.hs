module LALR where
import Grammar
import Utils

import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import qualified Data.Array as Array
import Data.Array(Array)

import Debug.Trace

data Lr0Item = Lr0Item !Int !Int deriving (Show,Eq,Ord) -- prodNo, dotPos
data Lr1Item = Lr1Item !Int !Int !GrammarSym deriving (Show,Eq,Ord) 

type Lr0Set = (Set Lr0Item, [(SymId, Int)])
type Lr1Set = (Set Lr1Item, [(SymId, Int)])

type GotoState = [(SymId, Int)]
type GotoTable = Array Int GotoState -- state -> [ nt, state ]
type ActionState = [(GrammarSym, ActionDef)]
data ActionDef = Shift Int {- state -} | Reduce Int {- prod -} | Accept deriving (Show,Eq)
type ActionTable = Array Int ActionState -- state -> [ term, action ]

-- CLOSURE for X -> . NT
lrClosure0 :: GrammarDef -> SymId -> [Lr0Item]
lrClosure0 gr = \sym -> fromJust$ lookup sym db
	where 
		db :: [(SymId, [Lr0Item])]
		db = map (\(nt, set) -> (nt, makeItems set)) db'
		db' = makeClosure step start
		
		step cur = map (close cur) cur
		
		close :: [(SymId, IntSet)] -> (SymId, IntSet) -> (SymId, IntSet)
		close st (sym, cur) = (sym, cur `IntSet.union` rules) where
			firsts = (mapMaybe getNt . mapMaybe (maybeHead.prodBody.prodNo)) (prodsFor sym)
			rules = foldl' IntSet.union IntSet.empty $ map (fromJust . flip lookup st) firsts
			
		start = map (\i -> (i, IntSet.fromList$ prodsFor i)) (nts gr)
		
		prodNo = prodByNo gr
		prodsFor = prodsByNt gr
		makeItems set = map (\e -> Lr0Item e 0) (IntSet.elems set)

-- CLOSURE for an item set
lrClosure :: GrammarDef -> (SymId -> [Lr0Item]) -> Set Lr0Item -> Set Lr0Item
lrClosure gr forNt = unionMap close1 where
	close1 item = Set.fromList (item : closeSym (getSym0 gr item))
	closeSym x = case x of { NT n -> forNt n; _ -> [] }

getSym1 gr (Lr1Item pr dot _) = getSym' gr pr dot
getSym0 gr (Lr0Item pr dot) = getSym' gr pr dot
getSym' gr pr dot = (headOr Eps . drop dot . prodBody . prodByNo gr) pr
	
-- GOTO for set and symbol
lrGoto :: GrammarDef -> SymId -> Set Lr0Item -> Set Lr0Item -- from closure to kernel
lrGoto gr sym = unionMap next where 
		next item@(Lr0Item pr dot) = case ptail item of
			(NT a:xs) | a == sym -> Set.singleton$ Lr0Item pr (dot+1)
			(Term a:xs) | a == sym -> Set.singleton$ Lr0Item pr (dot+1)
			_ -> Set.empty
		ptail (Lr0Item pr dot) = (drop dot . prodBody . prodByNo gr) pr

lrFirst :: GrammarDef -> [GrammarSym] -> Set GrammarSym
lrFirst gr = slideFirst (lrFirst0 gr)

lrFirst0 :: GrammarDef -> SymId -> Set GrammarSym -- FIRST :: non-term -> Term or Eps
lrFirst0 gr = fromJust . flip lookup db where
	db = makeClosure populate [(nt, Set.empty) | nt <- nts gr]
	populate st = map (grow st) st
	grow st (nt, cur) = (nt, un) where
		prods = map (prodByNo gr) (prodsByNt gr nt)
		bodies = map prodBody prods
		fn = fromMaybe Set.empty . flip lookup st
		un = Set.unions $ map (slideFirst fn) bodies
	
slideFirst :: (SymId -> Set GrammarSym) -> [GrammarSym] -> Set GrammarSym
slideFirst db = foldr combine (Set.singleton Eps) where
	combine sym set = case sym of
		NT n -> let s = db n in 
			if Eps `Set.member` s then Set.filter (/=Eps) s `Set.union` set	else s
		s -> Set.singleton s

makeCollection :: GrammarDef -> [Lr0Set]
makeCollection gr = reverse$ populate [] [Set.singleton (Lr0Item 0 0)] where
	closure = lrClosure gr (lrClosure0 gr)
	populate :: [Lr0Set] -> [Set Lr0Item] -> [Lr0Set]
	populate have [] = have
	populate have q@(item:rest) = populate ((item, gotos):have) (tail q2) where
		(gotos, q2) = foldr getGoto ([], q) (syms gr)
		getGoto sym (cl, cq) = if Set.null dest then (cl, cq) 
				else ((sym, idx):cl, augQueue) where
			dest = lrGoto gr sym (closure item)
			slot = length have + length cq
			inQueueRaw = elemIndex dest cq
			inQueue = fmap (+ length have) inQueueRaw
			inHaveRaw = findIndex (\(set, _) -> set == dest) have
			inHave = fmap (length have - 1 -) inHaveRaw
			idx = fromMaybe (fromMaybe slot inQueue) inHave
			augQueue = if (inQueue /= Nothing) || (inHave /= Nothing) 
				then cq else cq ++ [dest]
			
lrClosure1 :: GrammarDef -> ([GrammarSym] -> Set GrammarSym) -> Set Lr1Item -> Set Lr1Item
lrClosure1 gr first = makeClosure closeStep where
	closeStep all = all `Set.union` unionMap closeItem all -- repeat until nothing new is added
	closeItem (Lr1Item pr dot la) = case prodTail of -- for each item in set
			l@(NT _:xs) -> closeSeq l la
			_ -> Set.empty
		where prodTail = (drop dot . prodBody . prodByNo gr) pr
	closeSeq (NT nt:beta) a = Set.fromList items where
		las = (Set.toList . first) (beta ++ [a])
		prods = prodsByNt gr nt
		items = [ Lr1Item pr 0 la | la <- las, pr <- prods]

type SpontLA = (Int, Lr0Item, GrammarSym)
spontaneousLookaheads :: GrammarDef -> ([GrammarSym] -> Set GrammarSym) -> [Lr0Set] -> [SpontLA]
spontaneousLookaheads gr first coll = (0, Lr0Item 0 0, Eps) : (concatMap processSet coll) where
	processSet :: Lr0Set -> [(Int, Lr0Item, GrammarSym)]
	processSet (set, goto) = concatMap (processItem goto) (Set.toList set)
	processItem :: [(SymId, Int)] -> Lr0Item -> [(Int, Lr0Item, GrammarSym)]
	processItem goto (Lr0Item pr dot) = map makeInfo $ filter (not.isPropagation) (Set.toList j) where
		j = closure$ Set.singleton (Lr1Item pr dot Dummy)
		isPropagation (Lr1Item _ _ la) | la == Dummy = True
		isPropagation _ = False
		makeInfo (Lr1Item pr dot la) = (fromJust$ lookup s goto, Lr0Item pr (dot+1), la)
			where s = symId $ getSym' gr pr dot
	closure = lrClosure1 gr first
	
type PropLA = (Int, Lr0Item, Int, Lr0Item)
propagatedLookaheads :: GrammarDef -> ([GrammarSym] -> Set GrammarSym) -> [Lr0Set] -> [PropLA]
propagatedLookaheads gr first coll = concatMap processSet (zip coll [0..]) where
	processSet :: (Lr0Set, Int) -> [(Int, Lr0Item, Int, Lr0Item)]
	processSet ((set, goto), no) = concatMap (processItem no goto) (Set.toList set)
	processItem :: Int -> [(SymId, Int)] -> Lr0Item -> [(Int, Lr0Item, Int, Lr0Item)]
	processItem no goto p@(Lr0Item pr dot) = mapMaybe makeInfo (Set.toList j) where
		j = closure$ Set.singleton (Lr1Item pr dot Dummy)
		makeInfo (Lr1Item pr dot la) | la == Dummy && length (prodBody$ prodByNo gr pr) > dot = Just (no, p, to, Lr0Item pr (dot+1))
			where s = symId $ getSym' gr pr dot; to = fromJust$ lookup s goto
		makeInfo _ = Nothing
	closure = lrClosure1 gr first

computeLookaheads :: [PropLA] -> [SpontLA] -> Set SpontLA
computeLookaheads props sponts = makeClosure fill (Set.fromList sponts) where
	fill :: Set SpontLA -> Set SpontLA
	fill cur = cur `Set.union` (unionMap prop cur)
	prop :: SpontLA -> Set SpontLA
	prop (setNo, item, sym) = Set.fromList $ map makeLA props where
		props = lookupProp setNo item
		makeLA (a, b, c, d) = (c, d, sym)
	lookupProp setNo item = filter f props where
		f r@(a, b, c, d) | a == setNo && b == item = True
		f _ = False

enrichItems :: Set SpontLA -> [Lr0Set] -> [Lr1Set]
enrichItems sponts sets = map conv (zip sets [0..]) where
	spontList = Set.toList sponts
	lookupLAs setNo r@(Lr0Item pr dot) = map (\(_, _, la) -> Lr1Item pr dot la) $ filter f spontList
		where f (a, b, c) | a == setNo && b == r = True; f _ = False
		
	conv :: (Lr0Set, Int) -> Lr1Set
	conv ((items, goto), no) = (items1, goto) where
		items1 = unionMap (Set.fromList . lookupLAs no) items

makeGotoTable :: GrammarDef -> [Lr1Set] -> GotoTable
makeGotoTable gr coll = Array.array (0, length coll - 1) $ map gather (zip coll [0..]) where
	gather :: (Lr1Set, Int) -> (Int, [(SymId, Int)])
	gather ((_, g), no) = (no, ntg) where
		ntg = filter hasNt g
		hasNt (s, _) = s `elem` nts gr

makeActionTable :: GrammarDef -> [Lr1Set] -> ActionTable
makeActionTable gr coll = Array.array (0, length coll -1) $ map gather (zip coll [0..]) where
	gather :: (Lr1Set, Int) -> (Int, [(GrammarSym, ActionDef)])
	gather ((s, g), no) = (no, nub actions) where
		actions = mapMaybe conv (Set.toList$ closure s)
		conv (Lr1Item 0 1 Eps) = Just (Eps, Accept)
		conv (Lr1Item pr dot la) | canReduce pr dot = Just (la, Reduce pr)
		conv z@(Lr1Item pr dot la) | expectTerm pr dot = Just (getSym1 gr z, Shift (getGoto g pr dot))
		conv _ = Nothing
	expectTerm pr dot = isTerm$ getSym' gr pr dot
	getGoto g pr dot = fromJust$ lookup (symId$ getSym' gr pr dot) g
	canReduce pr dot = dot == (length $ prodBody (prodByNo gr pr))
	closure = lrClosure1 gr first where first = lrFirst gr
