module Moody where
import Grammar
import Data.IntSet(IntSet)
import qualified Data.IntSet as IntSet
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe
import Utils
import Data.List

data Lr0Item = Lr0Item !Int !Int deriving (Show, Eq, Ord) -- prodNo, dotPos

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
			firsts = (mapMaybe getNt . map (head.prodBody.prodNo)) (prodsFor sym)
			rules = foldl' IntSet.union IntSet.empty $ map (fromJust . flip lookup st) firsts
			
		start = map (\i -> (i, IntSet.fromList$ prodsFor i)) (nts gr)
		
		prodNo = prodByNo gr
		prodsFor = prodsByNt gr
		makeItems set = map (\e -> Lr0Item e 0) (IntSet.elems set)


lrClosure :: GrammarDef -> (SymId -> [Lr0Item]) -> Set Lr0Item -> Set Lr0Item
lrClosure gr forNt = Set.unions . map close1 . Set.elems where
	close1 item = Set.fromList (item : closeSym (getSym item))
	closeSym (NT n) = forNt n
	closeSym _ = []
	getSym (Lr0Item pr dot) = (safeHead Eps . drop dot . prodBody . prodByNo gr) pr
	safeHead d l = case l of { x:xs -> x; _ -> d }
