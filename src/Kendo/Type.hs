
{-# LANGUAGE OverloadedStrings #-}

module Kendo.Type (
      Type(..)
    , Kind(..)
    , TVar(..)
    , Pred(..)
    , TyCon(..)
    ) where


data Type
    = TVar TVar
    | TCon TyCon
    | TApp Type Type
    | TArr Type Type
    | TForall [Pred] [TVar] Type
    deriving (Show, Eq, Ord)

data Kind
    = KStar
    | KArr Kind Kind
    | KPrim
    | KVar String
    deriving (Show, Eq, Ord)

data TyCon
    = AlgTyCon { tyId :: String }
    | PrimTyCon { tyId :: String }
    deriving (Show, Eq, Ord)

data Pred
    = IsIn String Type
    deriving (Show, Eq, Ord)

data TVar = TV
    { tvName :: String
    } deriving (Show, Eq, Ord)

