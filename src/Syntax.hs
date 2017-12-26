module Syntax where

data TopLevel
  = Definition Identifier (Maybe Type) Value 
  | Runnable Expression
  deriving (Show)

data Type
  = TyInt
  | TyBool
  | TyUnit
  | TyArrow Type Row Type
  deriving (Show)

data Row
  = RVar Identifier
  | Open [Identifier] Identifier
  | Closed [Identifier]
  deriving (Show)

data Value
  = Lambda Identifier Expression
  | Lit Literal
  | Variable Identifier
  deriving (Show)

data Expression
  = Application Expression Expression
  | Val Value
  | Lift Expression
  | HandleExp Expression [ Handler ]
  deriving (Show)

data Literal
  = I Int
  | B Bool  
  | S String
  | U
  deriving (Show)

data Handler
  = Handler Identifier Identifier Identifier Expression
  deriving (Show)

type Identifier = String

