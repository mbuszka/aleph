module Builtins where

import Control.Monad.Writer

import qualified Data.List as List
import qualified Data.Map  as Map
import           Data.Map(Map)

import Syntax.Grammar
import Inference.Env    as IEnv
import Evaluation.Env   as EEnv
import Evaluation.Types

opTypes :: Map Ident (TyLit, Typ, Typ)
opTypes = Map.fromList
  [ (ID "print", (TL "IO", tl "Int",  tl "Unit"))
  ]
  where tl = TyLit . TL

effects :: Map TyLit [Ident]
effects = Map.fromList
  [ (TL "IO", [ID "print"])
  ]

primTypes :: Map Ident Scheme
primTypes = Map.fromList
  [ (ID "print", Scheme [TV "a"] $ TyArr int (row "a") unit)
  , (ID "add", iii)
  , (ID "sub", iii)
  , (ID "mul", iii)
  , (ID "div", iii)
  , (ID "isZero", ib)
  , (ID "nil", Scheme [] list)
  , (ID "cons", Scheme [TV "a", TV "b"] $ TyArr int (row "a") (TyArr list (row "b") list))
  , (ID "null", Scheme [TV "a"] $ TyArr list (row "a") bool)
  , (ID "head", Scheme [TV "a"] $ TyArr list (row "a") int)
  , (ID "tail", Scheme [TV "a"] $ TyArr list (row "a") list) 
  ] where
      int = TyLit $ TL "Int"
      list = TyLit $ TL "List"
      bool = TyLit $ TL "Bool"
      unit = TyLit $ TL "Unit"
      row = Row [] . Just . TV
      iii = Scheme [TV "a", TV "b"] $ TyArr int (row "a") (TyArr int (row "b") int)
      ib  = Scheme [TV "a"] $ TyArr int (row "a") bool

typeEnv :: IEnv.Env
typeEnv = IEnv.Env primTypes opTypes effects

evalEnv :: EEnv.Env
evalEnv = EEnv.Env $ Map.fromList
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

evalCtx :: Ctx
evalCtx = Ctx 
  [ HandlerFrame (TL "IO")
      (\v c -> apply c v)
      (Map.fromList 
        [ (ID "print", 
          \res v ctx -> tell [v] >> apply (Ctx $ res ++ _contStack ctx) UnitVal)
        ])
  ]
