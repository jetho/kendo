module Kendo.Ast 
    ( Exp(..)
    , Lit(..)
    , BinOp(..)
    , Decl(..)
    , Pattern(..)
    , isVal
    , int
    , float
    , string
    , char
    , trueVal
    , falseVal
    , bool
    , lam
    , (@@)
    , app
    ) where


type Id = String

data Exp
    = Literal Lit
    | Bin Exp BinOp Exp
    | Var Id
    | Lam Id Exp
    | App Exp Exp
    | Tuple [Exp]
    | Nth Int Exp
    | Rec [(Id, Exp)]
    | Field Int Exp
    | Con Id (Maybe Exp)
    | Case Exp [(Pattern, Exp)]
    | Let Decl Exp
    | Fun Id Id Exp
    deriving (Eq, Show)

data Lit 
    = LInt Int
    | LFloat Float
    | LString String
    | LChar Char
    deriving (Eq, Show)

data BinOp 
    = Plus 
    | Times 
    | Minus 
    | Concat 
    | Equal
    deriving (Eq, Show)

data Decl 
    = ValDecl Pattern Exp
    | FunDecl Id Id Exp
    deriving (Eq, Show)

data Pattern 
    = PWildcard
    | PId Id
    | PLit Lit
    | PCon Id (Maybe Pattern)
    | PTuple [Pattern]
    | PRec [(Id, Pattern)]
    deriving (Eq, Show)

isVal :: Exp -> Bool
isVal (Literal _)      = True
isVal (Lam _ _)        = True
isVal (Tuple es)       = all isVal es
isVal (Rec fields)     = all (isVal . snd) fields
isVal (Con _ Nothing)  = True
isVal (Con _ (Just e)) = isVal e
isVal _                = False

trueVal, falseVal :: Exp
trueVal  = Con "true"  Nothing
falseVal = Con "false" Nothing

bool :: Bool -> Exp
bool True  = trueVal
bool False = falseVal

int :: Int -> Exp
int = Literal . LInt

float :: Float -> Exp
float  = Literal . LFloat

string :: String -> Exp
string = Literal . LString

char :: Char -> Exp
char   = Literal . LChar

lam :: [Id] -> Exp -> Exp
lam xs e = foldr Lam e xs

infixl 9 @@
(@@) :: Exp -> Exp -> Exp
e1 @@ e2 = App e1 e2

app :: [Exp] -> Exp
app [] = error "app applied to empty list"
app xs = foldl1 (@@) xs

