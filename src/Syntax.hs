{-# LANGUAGE DeriveDataTypeable #-}

module Syntax where

import Data.Generics
import Data.Set(Set)

data Kind
  = KRow
  | KType
  deriving (Show, Data)

data TypeBinding = TBind Identifier Kind deriving (Show, Data)
data VarBinding = VBind Identifier (Maybe Type) deriving (Show, Data)

data Binding
  = BVar VarBinding
  | BTyp TypeBinding
  deriving (Show, Data)

data TopLevel
  = Definition VarBinding Value
  | Runnable Expression
  deriving (Show, Typeable, Data)

data Type
  = TyLit Identifier
  | TyArrow Type Row Type
  | TyForall TypeBinding Type
  deriving (Show, Typeable, Data)

data Row
  = ROpen [Identifier] Identifier
  | RClosed [Identifier]
  deriving (Show, Typeable, Data)

data Value
  = Lambda Binding Expression
  | Lit Literal
  | Variable Identifier
  deriving (Show, Typeable, Data)

data Expression
  = App Expression Expression
  | RowExp Row
  | Val Value
  | Lift Identifier Expression
  | HandleExp Expression [ Handler ]
  | LetExp VarBinding Expression Expression
  deriving (Show, Typeable, Data)

data Literal
  = I Int
  | B Bool  
  | S String
  | U
  deriving (Show, Typeable, Data)

data Handler
  = Handler Identifier VarBinding Identifier Expression
  deriving (Show, Typeable, Data)

type Identifier = String

