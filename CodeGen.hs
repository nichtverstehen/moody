module CodeGen where
import LALR
import Grammar

generateCode :: GrammarDef -> GotoTable -> ActionTable -> String
generateCode gr goto action = intro gr
