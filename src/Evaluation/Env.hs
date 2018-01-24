module Evaluation.Env where

import Control.Lens
import Control.Monad.Reader

import Data.Map as Map

import Syntax.Grammar(Ident(..), TyLit(..))
import Evaluation.Types
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

init :: Env
init = Env $ Map.fromList
  [ (ID "print", OpVal (ID "print") (TL "IO"))
  , (ID "add", 
    FunVal $ 
      \case (IntVal a) -> \c -> apply c $
              FunVal (\case (IntVal b) -> \c -> apply c $ IntVal (a + b)))
  , (ID "sub", 
    FunVal $ 
      \case (IntVal a) -> \k -> apply k $ 
              FunVal (\case (IntVal b) -> \k -> apply k $ IntVal (a - b)))
  , (ID "mul", 
    FunVal $ 
      \case (IntVal a) -> \k -> apply k $ 
              FunVal (\case (IntVal b) -> \k -> apply k $ IntVal (a * b)))
  , (ID "div", 
    FunVal $ 
      \case (IntVal a) -> \k -> apply k $ 
              FunVal (\case (IntVal b) -> \k -> apply k $ IntVal (a `div` b)))
  , (ID "isZero",
    FunVal $
      \case (IntVal a) -> \k -> apply k $ BoolVal (a == 0))
  , (ID "nil", ListVal [])
  , (ID "cons", FunVal $ 
      \case (IntVal a) -> \k -> apply k $ 
              FunVal (\case (ListVal b) -> \k -> apply k $ ListVal (a : b)))
  , (ID "null", FunVal $ \case ListVal l -> \k -> apply k $ BoolVal (Prelude.null l))
  , (ID "head", FunVal $ \case ListVal (h:_) -> \k -> apply k (IntVal h))
  , (ID "tail", FunVal $ \case ListVal (_:t) -> \k -> apply k (ListVal t))
  ]