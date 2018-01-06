{-# LANGUAGE FlexibleContexts #-}

module Error where

import Control.Monad.Except

data Error
  = ParseError String
  | UnboundVariable String
  | TypeError String
  | KindError String
  | UnificationError String
  deriving Show

throw :: MonadError Error m => Error -> m a
throw = throwError