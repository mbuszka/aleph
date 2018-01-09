{-# LANGUAGE FlexibleContexts #-}

module Error where

import Control.Monad.Except

import Grammar

data Error
  = ParseError String
  | UnboundVariable String
  | UnknownOperation Ident
  | TypeError String
  | KindError String
  | UnificationError String
  deriving Show

throw :: MonadError Error m => Error -> m a
throw = throwError