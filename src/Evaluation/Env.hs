module Evaluation.Env where

import Data.Map as Map

import Evaluation.Types
import Syntax.Grammar
import Print

newtype Env = Env { unEnv :: Map Ident ExpVal }

lookup :: MonadEval m => Ctx -> Ident -> Env -> m ExpVal
lookup ctx v e = case Map.lookup v $ unEnv e of
  Just val -> return val
  Nothing  -> abort ctx $ "Unbound variable:" <+> pretty v

extend :: Ident -> ExpVal -> Env -> Env
extend id v = Env . Map.insert id v . unEnv

empty :: Env
empty = Env Map.empty
