module Syntax where

import Type

type Identifier = String

type Program = [Top]

data Top
  = TLet Binding Term
  | TRun Term
  deriving (Show)

data Binding
  = Bind Var (Maybe Type)
  deriving (Show)

newtype Var = V String
  deriving (Show, Eq, Ord)

data Term
  = App Term Term
  | Let Binding Term Term
  | Abs Binding Term
  | Lit Val
  | Var Var
  | Handler Var Var Var Term
  | Ret Var Term
  | Handle Term [Term]
  | Lift String Term
  deriving (Show)

data Val
  = VBool Bool
  | VInt Integer
  | VUnit
  deriving (Show)
