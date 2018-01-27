module Syntax 
  ( Syntax.parse
  , Handler(..)
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

import Control.Monad.Except

import Data.Text as Text

import Syntax.Grammar 
import Syntax.Parse   as Parse

import Error

parse :: (MonadError Error m) => Text -> m [Top]
parse = Parse.parse program