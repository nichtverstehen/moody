module Moody where
import Grammar
import Utils

import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List

import Debug.Trace

data Lr0Item = Lr0Item !Int !Int deriving (Show,Eq,Ord) -- prodNo, dotPos
data Lr1Item = Lr1Item !Int !Int !SymId deriving (Show,Eq,Ord) 

type Lr0Set = (Set Lr0Item, [(SymId, Int)])

-- CLOSURE for A -> . NT
lrClosure0 :: GrammarDef -> SymId -> [Lr0Item]
lrClosure0 gr = \sym -> fromJust$ lookup sym db
	where 
		db :: [(SymId, [Lr0Item])]
		db = map (\(nt, set) -> (nt, makeItems set)) db'
		db' = makeClosure step start
		
		step cur = map (close cur) cur
		
		close :: [(SymId, IntSet)] -> (SymId, IntSet) -> (SymId, IntSet)
		close st (sym, cur) = (sym, cur `IntSet.union` rules) where
			firsts = (mapMaybe getNt . mapMaybe (safeHead.prodBody.prodNo)) (prodsFor sym)
			rules = foldl' IntSet.union IntSet.empty $ map (fromJust . flip lookup st) firsts
			
		start = map (\i -> (i, IntSet.fromList$ prodsFor i)) (nts gr)
		
		prodNo = prodByNo gr
		prodsFor = prodsByNt gr
		makeItems set = map (\e -> Lr0Item e 0) (IntSet.elems set)
		safeHead l = if null l then Nothing else Just (head l)

-- CLOSURE for an item set
lrClosure :: GrammarDef -> (SymId -> [Lr0Item]) -> Set Lr0Item -> Set Lr0Item
lrClosure gr forNt = Set.unions . map close1 . Set.elems where
	close1 item = Set.fromList (item : closeSym (getSym item))
	closeSym (NT n) = forNt n
	closeSym _ = []
	getSym (Lr0Item pr dot) = (safeHead Eps . drop dot . prodBody . prodByNo gr) pr
	safeHead d l = case l of { x:xs -> x; _ -> d }

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
		s@(Term _) -> Set.singleton s


makeCollection :: GrammarDef -> [Lr0Set]
makeCollection gr = reverse$ populate [] [Set.singleton (Lr0Item 0 0)] where
	closure = lrClosure gr (lrClosure0 gr)
	populate :: [Lr0Set] -> [Set Lr0Item] -> [Lr0Set]
	populate have [] = have
	populate have q@(item:rest) = populate ((item, gotos):have) (tail q2) where
		(gotos, q2) = foldr getGoto ([], q) (syms gr)
		getGoto sym (cl, cq) = if Set.null dest then (cl, cq) 
				else ((sym, idx):cl, augQueue) where
			dest = goto gr sym (closure item)
			slot = length have + length cq
			inQueueRaw = elemIndex dest cq
			inQueue = fmap (+ length have) inQueueRaw
			inHaveRaw = findIndex (\(set, _) -> set == dest) have
			inHave = fmap (length have - 1 -) inHaveRaw
			idx = fromMaybe (fromMaybe slot inQueue) inHave
			augQueue = if (inQueue /= Nothing) || (inHave /= Nothing) 
				then cq else cq ++ [dest]
			
