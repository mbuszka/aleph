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

import Control.Concurrent
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

defaultHandlers :: [Cont]
defaultHandlers =
  [ HandlerFrame (TL "IO")
      (\v c -> apply c v)
      (M.fromList 
        [ (ID "print", 
          \res v ctx -> tell [v] >> apply (Ctx $ res ++ _contStack ctx) UnitVal)
        ])
  ]

initialCtx :: Ctx
initialCtx = Ctx defaultHandlers

evalProgram :: (MonadEval m) => [Top] -> m ()
evalProgram = evalP Env.init initialCtx

evalP :: MonadEval m => Env -> Ctx -> [Top] -> m ()
evalP e c (Def x t : ts) = do
  v <- eval e t c
  evalP (Env.extend x v e) c ts
evalP e c (Run t : ts) = eval e t c >> evalP e c ts
evalP env c (EffDef l ops : ts) =
  let env' = foldl (\e -> \case (OpDef x _ _) -> Env.extend x (OpVal x l) e) env ops
  in evalP env' c ts
evalP _ _ [] = return ()

splitHandlers :: [Handler] -> (Maybe (Ident, Term), Map Ident (Ident, Ident, Term))
splitHandlers = let
  p (ret, ops) (Op i v k t) = (ret, M.insert i (v, k , t) ops)
  p (_  , ops) (Ret    v t) = (Just (v, t), ops)
  in foldl p (Nothing, M.empty)

find p (x:xs) = case p x of
  Just y  -> Just y
  Nothing -> find p xs

eval :: MonadEval m => Env -> Term -> Ctx -> m ExpVal
eval env t ctx = do
  -- liftIO $ putDocW 80 $ pretty t <> line
  -- liftIO $ threadDelay 1000000
  case t of
    Var v -> Env.lookup ctx v env >>= apply ctx
    Lit v -> case v of
      Grammar.VInt i  -> apply ctx $ IntVal i
      Grammar.VUnit   -> apply ctx UnitVal
      Grammar.VBool b -> apply ctx $ BoolVal b
    Abs id exp -> apply ctx $ FunVal (\v -> eval (Env.extend id v env) exp)
    App fun exp ->
      eval env fun $ pushFrame (RegularFrame $ \case 
        FunVal f   -> \c -> eval env exp $ pushFrame (RegularFrame f) c
        OpVal id l -> eval env exp . pushFrame (RegularFrame $ \v c -> do
          -- liftIO $ putDocW 80 $ "handling" <+> pretty id <> line <> "stack:" <+> pretty c <> line
          (resumption, hs, rest) <- splitOn l c
          h <- getOrErr c id hs
          h resumption v (Ctx rest))
        ResVal cs -> eval env exp . pushFrame (RegularFrame $ \v -> \case
          Ctx c -> do
            -- liftIO $ putDocW 80 $ "calling resumption with:" <> line
            --   <> "top:" <+> pretty cs <> line
            --   <> "stack:" <+> pretty c <> line
            apply (Ctx (cs ++ c)) v)
        v -> \c -> abort c $ "Unexpected value:" <+> pretty v <+> "function required.") ctx
    Let id body exp ->
      eval env body $ pushFrame (RegularFrame $ \v c -> eval (Env.extend id v env) exp c) ctx
    LetRec f x body exp ->
      let env' = Env.extend f fun env
          fun  = FunVal $ \v c -> eval (Env.extend x v env') body c
      in  eval env' exp ctx
    Cond c t1 t2 ->
      eval env c $ pushFrame (RegularFrame $
        \case BoolVal b -> \c ->
                if b then eval env t1 c else eval env t2 c) ctx
    Bind id e1 e2 ->
      eval env e1 $ pushFrame (RegularFrame $ \v c -> eval (Env.extend id v env) e2 c) ctx
    Handle lbl exp hs ->
      let (Just (rx, rt), ops) = splitHandlers hs
          ret :: forall m. MonadEval m => Fun m
          ret v = eval (Env.extend rx v env) rt
          
          prep :: forall m. MonadEval m => (Ident, Ident, Term) -> [Cont] -> Fun m
          prep (x, r, t) cs v =
            eval (Env.extend x v . Env.extend r (ResVal cs) $ env) t
          
          ops' :: forall m. MonadEval m => Handlers m
          ops' = fmap prep ops
      in  eval env exp $ pushFrame (HandlerFrame lbl ret ops') ctx
    Lift lbl exp -> eval env exp $ pushFrame (LiftFrame lbl) ctx
