module Kendo.Eval 
    ( 
    ) where

import Kendo.Ast


applyBinOp :: BinOp -> Lit -> Lit -> Exp
applyBinOp Plus   (LInt x)     (LInt y)     = int $ x+y
applyBinOp Plus   (LFloat x)   (LFloat y)   = float $ x+y
applyBinOp Times  (LInt x)     (LInt y)     = int $ x*y
applyBinOp Times  (LFloat x)   (LFloat y)   = float $ x*y
applyBinOp Minus  (LInt x)     (LInt y)     = int $ x-y
applyBinOp Minus  (LFloat x)   (LFloat y)   = float $ x-y
applyBinOp Concat (LString s1) (LString s2) = string $ s1 ++ s2
applyBinOp Equal  (LInt x)     (LInt y)     = bool $ x==y
applyBinOp Equal  (LFloat x)   (LFloat y)   = bool $ x==y 
applyBinOp Equal  (LString s1) (LString s2) = bool $ s1==s2
applyBinOp Equal  (LChar c1)   (LChar c2)   = bool $ c1==c2
applyBinOp _      _            _            = error "Bad binary application"
