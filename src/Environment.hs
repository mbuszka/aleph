{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Environment where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

import qualified Data.Map as M
import           Data.Map (Map)

import Error
import Grammar

data Scheme = Scheme [TVar] Typ
  deriving Show

data Environment = Env
  { _eTypeContext :: Map Ident Scheme
  } deriving (Show)

makeLenses ''Environment

lookup :: (MonadReader Environment m, MonadError Error m) => Ident -> m Scheme
lookup v = do
  ms <- asks (\env -> env ^. eTypeContext . to (M.lookup v))
  case ms of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable (show v)


inEnv :: (MonadReader Environment m) => Ident -> Scheme -> m a -> m a
inEnv v s = local (eTypeContext %~ M.insert v s)
