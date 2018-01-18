{-# LANGUAGE 
    FlexibleContexts
  , OverloadedStrings
  , RankNTypes
#-}

module Error where

import           Control.Monad.Except

import           Data.Text.Prettyprint.Doc

import Syntax.Grammar
import Print

data Error
  = ParseError String
  | UnboundVariable Ident
  | UnknownOperation Ident
  | UnknownEffect TyLit
  | TypeError (forall a. Doc a)
  | KindError (forall a. Doc a)
  | UnificationError (forall a. Doc a)
  | RuntimeError (forall a. Doc a)

instance Pretty Error where
  pretty (ParseError s) = "Parse error:" <+> pretty s
  pretty (UnboundVariable s) = "Unbound identifier:" <+> pretty s
  pretty (UnknownOperation i) = "Unknown operation:" <+> pretty i
  pretty (UnknownEffect e) = "Unknown effect:" <+> pretty e
  pretty (TypeError s) = "Type error:" <+> s
  pretty (KindError s) = "Kind error" <+> s
  pretty (UnificationError s) = "Unification error:" <+> s
  pretty (RuntimeError s) = "Runtime error:" <+> s

throw :: MonadError Error m => Error -> m a
throw = throwError