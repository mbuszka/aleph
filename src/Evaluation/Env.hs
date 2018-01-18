module Evaluation.Env where

import Control.Lens
import Control.Monad.Reader

import Data.Map as Map

import Syntax.Grammar(Ident)
import Evaluation.Types
import Print

newtype Env = Env { unEnv :: Map Ident ExpVal }

lookup :: MonadEval m => Ident -> Env -> m ExpVal
lookup v e = case Map.lookup v $ unEnv e of
    Just val -> return val
    Nothing  -> do
      mo <- asks (\e -> e ^. effects . at v)
      case mo of
        Just lbl -> return $ OpVal v lbl
        Nothing  ->  abort $ "Unbound variable:" <+> pretty v

extend :: Ident -> ExpVal -> Env -> Env
extend id v = Env . Map.insert id v . unEnv

empty :: Env
empty = Env Map.empty