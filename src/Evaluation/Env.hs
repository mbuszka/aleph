module Evaluation.Env where

import Control.Lens
import Control.Monad.Reader

import Data.Map as Map

import Syntax.Grammar(Ident(..))
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

init :: Env
init = Env $ Map.fromList
  [ (ID "add", 
    FunVal $ 
      \case (IntVal a) -> \k -> apply k $ 
              FunVal (\case (IntVal b) -> \k -> apply k $ IntVal (a + b)))
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
  ]