module Syntax.Grammar 
  ( Handler(..)
  , Ident(..)
  , OpDef(..)
  , Row(..)
  , Term(..)
  , Top(..)
  , Typ(..)
  , TyLit(..)
  , TyVar(..)
  , Val(..)
  ) where

import Data.Text

newtype TyVar = TV Text deriving (Show, Eq, Ord)
newtype TyLit = TL Text deriving (Show, Eq, Ord)
newtype Ident = ID Text deriving (Show, Eq, Ord)

data Top
  = Def Ident Term
  | Run Term
  | EffDef TyLit [OpDef]
  deriving Show
  
data OpDef = OpDef Ident Typ Typ
  deriving Show

data Term
  = App Term Term
  | Let Ident Term Term
  | LetRec Ident Ident Term Term
  | Abs Ident Term
  | Cond Term Term Term
  | Var Ident
  | Lit Val
  | Handle TyLit Term [Handler]
  | Lift TyLit Term
  | Bind Ident Term Term
  deriving Show

data Handler
  = Op  Ident Ident Ident Term
  | Ret Ident Term
  deriving Show

data Val
  = VInt Integer
  | VBool Bool
  | VUnit
  deriving Show

data Typ
  = TyArr Typ Row Typ
  | TyVar TyVar
  | TyLit TyLit
  deriving (Eq, Ord, Show)

data Row = Row [TyLit] (Maybe TyVar)
  deriving (Eq, Ord, Show)
