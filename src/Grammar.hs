module Grammar 
  ( Top(..)
  , Term(..) 
  , Handler(..)
  , OpDef(..)
  , Val(..)
  , Typ(..)
  , Row(..)
  , TyVar(..)
  , TyLit(..)
  , Ident(..)
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
  | Abs Ident Term
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
  | VUnit
  deriving Show

data Typ
  = TyArr Typ Row Typ
  | TyVar TyVar
  | TyLit TyLit
  deriving (Eq, Ord, Show)

data Row = Row [TyLit] (Maybe TyVar)
  deriving (Eq, Ord, Show)
