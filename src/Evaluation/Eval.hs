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
  => Ctx -> ReaderT Ctx (WriterT [ExpVal] m) a -> m (a, [ExpVal])
runEval e c = runWriterT (runReaderT c e)

defaultHandlers :: Map TyLit [Handlers]
defaultHandlers = M.fromList
  [ (TL "IO", [ Handlers 
      (M.fromList 
        [ (ID "print", \v k -> tell [v] >> apply k UnitVal)
        ])])
  ]

initialCtx :: Map Ident TyLit -> Ctx
initialCtx = Ctx defaultHandlers

evalProgram :: (MonadEval m) => [Top] -> m ()
evalProgram = evalP Env.empty

evalP :: MonadEval m => Env -> [Top] -> m ()
evalP e (Def x t : ts) = do
  v <- eval e t finalCont
  evalP (Env.extend x v e) ts
evalP e (Run t : ts) = eval e t finalCont >> evalP e ts
evalP e (EffDef{} : ts) = evalP e ts
evalP _ [] = return ()

finalCont :: Cont
finalCont = Cont $ \v -> return v

find p (x:xs) = case p x of
  Just y  -> Just y
  Nothing -> find p xs

prepareHandlers :: TyLit -> Env -> Cont -> [Handler] -> (Handlers, Cont)
prepareHandlers l e k hs =
  let Just (x, t) = 
        find (\case Ret x t -> Just (x, t)
                    _ -> Nothing) hs
      k' = Cont (\v -> eval (Env.extend x v e) t k)
  in  (Handlers 
        (M.fromList $ foldr (\x acc -> case x of
          Op id a cont t -> 
            (id, \v k -> 
              withoutHandlers l $ eval 
                (Env.extend a v . Env.extend cont (ContVal k) $ e) t k'
            ) : acc
          Ret _ _ -> acc) [] hs)
      , k')

eval :: MonadEval m => Env -> Term -> Cont -> m ExpVal
eval e t k = case t of
  Var v -> Env.lookup v e >>= apply k
  Lit v -> case v of
    Grammar.VInt i -> apply k $ IntVal i
    Grammar.VUnit  -> apply k UnitVal
  Abs id exp -> apply k $ FunVal (\v k -> eval (Env.extend id v e) exp k)
  App fun exp ->
    eval e fun $ Cont $ \case 
      FunVal f   -> eval e exp $ Cont (`f` k)
      OpVal id l -> eval e exp $ Cont (\v -> do
        h <- lookupHandler l id
        h v k)
      ContVal k  -> eval e exp k
      v -> abort $ "Unexpected value:" <+> pretty v <+> "function required."
  Let id body exp ->
    eval e body $ Cont $ \v -> eval (Env.extend id v e) exp k
  Bind id e1 e2 ->
    eval e e1 $ Cont $ \v -> eval (Env.extend id v e) e2 k
  Handle lbl exp hs ->
    let (hs', k') = prepareHandlers lbl e k hs
    in  withHandlers lbl hs' $ eval e exp k'

