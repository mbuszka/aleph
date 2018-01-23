{-# LANGUAGE
    GADTs
  , ConstraintKinds
  , FlexibleContexts
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , TemplateHaskell
#-}

module Evaluation.Eval where

import Control.Lens

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Except

import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Text as T
import           Data.Text(Text)

import Error
import Syntax.Grammar as Grammar hiding (Val)
import Evaluation.Types
import Evaluation.Env as Env
import Print

runEval :: (MonadError Error m, MonadIO m) 
  => WriterT [ExpVal] m a -> m (a, [ExpVal])
runEval = runWriterT

defaultHandlers :: Map TyLit [Handlers]
defaultHandlers = M.fromList
  [ (TL "IO", [ Handlers 
      (M.fromList 
        [ (ID "print", \v k -> tell [v] >> apply k UnitVal)
        ])])
  ]

initialCtx :: Ctx
initialCtx = Ctx defaultHandlers

evalProgram :: (MonadEval m) => [Top] -> m ()
evalProgram = evalP Env.init initialCtx

evalP :: MonadEval m => Env -> Ctx -> [Top] -> m ()
evalP e c (Def x t : ts) = do
  v <- eval e c t finalCont
  evalP (Env.extend x v e) c ts
evalP e c (Run t : ts) = eval e c t finalCont >> evalP e c ts
evalP env c (EffDef l ops : ts) =
  let env' = foldl (\e -> \case (OpDef x _ _) -> Env.extend x (OpVal x l) e) env ops
  in evalP env' c ts
evalP _ _ [] = return ()

finalCont :: Cont
finalCont = Cont $ \v -> return v

find p (x:xs) = case p x of
  Just y  -> Just y
  Nothing -> find p xs

prepareHandlers :: Env -> Ctx -> Cont -> [Handler] -> (Handlers, Cont)
prepareHandlers env ctx exitK hs =
  let Just (x, t) = 
        find (\case Ret x t -> Just (x, t)
                    _       -> Nothing
             ) hs
      returnK = Cont (\v -> eval (Env.extend x v env) ctx t exitK)
  in  (Handlers 
        (M.fromList $ foldr (\x acc -> case x of
          Op id a cont t -> 
            (id, \v k -> let
              env' = Env.extend a v . Env.extend cont (ContVal k) $ env
              in eval env' ctx t exitK
            ) : acc
          Ret _ _ -> acc) [] hs)
      , returnK)

eval :: MonadEval m => Env -> Ctx -> Term -> Cont -> m ExpVal
eval env ctx t k = case t of
  Var v -> Env.lookup ctx v env >>= apply k
  Lit v -> case v of
    Grammar.VInt i  -> apply k $ IntVal i
    Grammar.VUnit   -> apply k UnitVal
    Grammar.VBool b -> apply k $ BoolVal b
  Abs id exp -> apply k $ FunVal (\v c k -> eval (Env.extend id v env) c exp k)
  App fun exp ->
    eval env ctx fun $ Cont $ \case 
      FunVal f   -> eval env ctx exp $ Cont (\v -> f v ctx k)
      OpVal id l -> eval env ctx exp $ Cont (\v -> do
        h <- lookupHandler l id ctx
        h v k)
      ContVal k  -> eval env ctx exp k
      v -> abort ctx $ "Unexpected value:" <+> pretty v <+> "function required."
  Let id body exp ->
    eval env ctx body $ Cont $ \v -> eval (Env.extend id v env) ctx exp k
  LetRec f x body exp ->
    let env' = Env.extend f fun env
        fun  = FunVal $ \v c k -> eval (Env.extend x v env') c body k
    in  eval env' ctx exp k
  Cond c t1 t2 ->
    eval env ctx c $ Cont $ \case BoolVal b -> if b then eval env ctx t1 k else eval env ctx t2 k
  Bind id e1 e2 ->
    eval env ctx e1 $ Cont $ \v -> eval (Env.extend id v env) ctx e2 k
  Handle lbl exp hs ->
    let (hs', k') = prepareHandlers env ctx k hs
    in  eval env (insertHandlers lbl hs' ctx) exp k'
  Lift lbl exp ->
    let hs = _handlers ctx
    in  eval env (removeHandlers lbl ctx) exp k


