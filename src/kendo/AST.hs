module AST where


type Ident = String

data Exp 
    = EVar Ident
    | ELit Lit
    | EAbs Ident Exp
    | EApp Exp Exp
    | ELet [Decl] Exp
    | EIf Exp Exp Exp
    deriving Show


data Decl = Decl Ident Exp
          deriving Show


data Lit
   = LInt Int
   | LChar Char
   | LString String
   | LBool Bool
   deriving Show

