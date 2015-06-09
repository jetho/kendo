
{-# LANGUAGE OverloadedStrings #-}

module Kendo.Syntax (
      Module(..)
    , Decl(..)
    , ConDecl(..)
    , Expr(..)
    , Literal(..)
    , BindGroup(..)
    , Match(..)
    , Pattern(..)
    , Stmt(..)
    , FixitySpec(..)
    , Assoc(..)
    , Fixity(..)
    , Constr
    , Name
    , Precedence
    , ModuleName
    ) where

import Kendo.Type


type Constr     = String
type Name       = String
type Precedence = Integer
type ModuleName = ([Name], Name)


data Module = Module ModuleName [Decl] 
    deriving (Eq,Show)

data Decl
    = FunDecl BindGroup 
    | TypeDecl Type
    | DataDecl Constr [Name] [ConDecl] 
    | ClassDecl [Pred] Name [Name] [Decl] 
    | InstDecl [Pred] Name Type [Decl] 
    | FixityDecl FixitySpec 
    deriving (Eq, Show)

data ConDecl
    = ConDecl Constr Type
    | RecDecl Constr [(Name, Type)] Type
    deriving (Eq, Show, Ord)

data Expr
    = EApp Expr Expr
    | EVar Name
    | ELam Name Expr
    | ELit Literal
    | ELet Name Expr Expr
    | EIf Expr Expr Expr
    | ECase Expr [Match]
    | EAnn Expr Type
    | EDo [Stmt]
    | EFail
    deriving (Eq, Show)

data Literal
    = LitInt Integer
    | LitChar Char
    | LitString String
    deriving (Eq, Ord, Show)

data BindGroup = BindGroup
    { _matchName  :: Name
    , _matchPats  :: [Match]
    , _matchType  :: Maybe Type
    , _matchWhere :: [[Decl]]
    } deriving (Eq, Show)

data Match = Match
    { _matchPat  :: [Pattern]
    , _matchBody :: Expr
    } deriving (Eq, Show)

data Pattern
    = PVar Name
    | PCon Constr [Pattern]
    | PLit Literal
    | PWild
    deriving (Eq, Show)

data Stmt
    = Generator Pattern Expr
    | Qualifier Expr
    deriving (Eq, Show)

data FixitySpec = FixitySpec
    { fixityFix  :: Fixity
    , fixityName :: String
    } deriving (Eq, Show)

data Assoc = L | R | N
    deriving (Eq,Ord,Show)

data Fixity
    = Infix Assoc Precedence
    | Prefix Precedence
    | Postfix Precedence
    deriving (Eq,Ord,Show)

