{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings
#-}
 
module Inference.Infer where

import Control.Monad.Except
import Control.Monad.State hiding (State)
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))
import Control.Monad.RWS(evalRWST, runRWST, RWST(..))
import Control.Monad
import Control.Lens

import           Data.List((\\), sort)
import qualified Data.Map as Map
import           Data.Map(Map)
import           Data.Maybe(mapMaybe, isNothing)
import qualified Data.Set as Set
import           Data.Set(Set)
import qualified Data.Text as Text

import Inference.Env   as Env
import Inference.Types
import Inference.Solve
import Inference.Subst as Subst

import Builtins
import Error
import Print
import Syntax

initState = State 0 emptySubst

processProgram :: (MonadCheck m) => [Top] -> m Env
processProgram [] = ask
processProgram (t:ts) = do
  e <- processTop t
  local (\_ -> e) $ processProgram ts

processTop :: (MonadCheck m) => Top -> m Env
processTop (Def id t) = do
  (typ, cs) <- listen $ do
    (typ, env) <- infer t
    constrRow env (Row [] Nothing)
    return typ
  s <- gets _sSubst
  sub <- solve s cs
  sch <- canonicalize . generalize Env.empty =<< apply sub typ
  Env.extend id sch <$> (apply sub =<< ask)
processTop (Run t) = do
  (typ, cs) <- listen (fst <$> infer t)
  s <- gets _sSubst
  sub <- solve s cs
  apply sub =<< ask
processTop (EffDef lbl ops) = processEff lbl ops

processEff :: (MonadCheck m) => TyLit -> [OpDef] -> m Env
processEff lbl ops = do
  types <- Map.fromList <$> mapM 
    (\(OpDef i a b) -> do
      -- v <- fresh
      let r = Row [lbl] (Just (TV "a"))
      return (i, Scheme [TV "a"] $ TyArr a r b)) ops
  let operations = Map.fromList $ map (\(OpDef i a b) -> (i, (lbl, a, b))) ops
  let effects = Map.singleton lbl $ map (\(OpDef i _ _) -> i) ops
  combine (Env types operations effects) <$> ask

infer :: MonadCheck m => Term -> m (Typ, Row)
infer (Var v)       = (,) <$> lookupEnv v <*> freshRow
infer (Lit VInt{})  = (,) (TyLit $ TL "Int")  <$> freshRow
infer (Lit VUnit{}) = (,) (TyLit $ TL "Unit") <$> freshRow
infer (Lit VBool{}) = (,) (TyLit $ TL "Bool") <$> freshRow
infer (Abs v term) = do
  tv <- freshTyp
  (ty, row) <- Env.extended v (Scheme [] tv) $ infer term
  rv <- freshRow
  return (TyArr tv row ty, rv)
infer (App t1 t2) = do
  (ty1, e1) <- infer t1
  (ty2, e2) <- infer t2
  tv <- freshTyp
  tr <- freshRow
  constrTyp ty1 (TyArr ty2 tr tv)
  constrRow e1 e2
  constrRow tr e1
  return (tv, tr)
infer (Cond c t e) = do
  (t1, e1) <- infer c
  constrTyp t1 (TyLit $ TL "Bool")
  (t2, e2) <- infer t
  (t3, e3) <- infer e
  constrRow e1 e2
  constrRow e2 e3
  constrTyp t2 t3
  return (t3, e3)
infer (Bind v t1 t2) = do
  (ty1, e1) <- infer t1
  (ty2, e2) <- Env.extended v (Scheme [] ty1) $ infer t2
  constrRow e1 e2
  return (ty2, e2)
infer (Let v body exp) = do
  ((ty1, e1), cs) <- listen $ infer body
  s <- gets _sSubst
  s' <- solve s cs
  modify $ sSubst .~ s'
  constrRow e1 (Row [] Nothing)
  env <- ask
  s <- canonicalize $ generalize env ty1
  Env.extended v s $ infer exp
infer (LetRec f v body exp) = do
  tf <- freshTyp
  tv <- freshTyp
  ((t, e), cs) <- listen $ do
    (tr, er) <- Env.extended f (Scheme [] tf) $ Env.extended v (Scheme [] tv) $ infer body
    constrTyp tf (TyArr tv er tr)
    return (tf, (Row [] Nothing))
  s <- gets _sSubst
  s' <- solve s cs
  modify $ sSubst .~ s'
  env <- ask
  s <- canonicalize $ generalize env tf
  Env.extended f s $ infer exp
infer (Handle lbl t hs) = do
  (ty1, e1) <- infer t
  fr <- fresh
  constrRow e1 (Row [lbl] (Just fr))
  (tyR, eR) <- inferHandlers lbl (ty1, (Row [] (Just fr))) hs
  return (tyR, eR)
infer (Lift lbl t) = do
  fr <- fresh
  (ty, eff) <- infer t
  let r = Row [lbl] $ Just fr
  constrRow (Row [] $ Just fr) eff
  return (ty, r)

inferHandler :: MonadCheck m => TyLit -> (Typ, Typ, Row) -> Handler -> m (Maybe Ident, Typ)
inferHandler hLbl (resT, contT, resE) (Op id arg cont exp) = do
  (lbl, argT, retT) <- lookupOp id
  when (hLbl /= lbl) $ throw $ 
    TypeError $ "Could not match operation's effect" <+> pretty lbl
             <+> "with handler's effect" <+> pretty hLbl
  (ty, env) <- Env.extended arg (Scheme [] argT) 
               $ Env.extended cont (Scheme [] (TyArr retT resE contT)) $ infer exp
  constrRow env resE
  constrTyp ty  contT
  return (Just id, ty)
inferHandler hLbl (resT, contT, resE) (Ret val exp) = do
  (ty, env) <- Env.extended val (Scheme [] resT) $ infer exp
  constrRow env resE
  constrTyp ty contT
  return (Nothing, ty)

inferHandlers :: MonadCheck m => TyLit -> (Typ, Row) -> [Handler] -> m (Typ, Row)
inferHandlers lbl (resT, resE) hs = do
  contT <- freshTyp
  types <- mapM (inferHandler lbl (resT, contT, resE)) hs
  desired <- lookupEff lbl
  -- liftIO $ putDocW 80 $ pretty desired
  let idents = mapMaybe fst $ types
  let ret    = filter isNothing . map fst $ types
  when (sort idents /= desired || ret /= [Nothing]) $ throw $
    TypeError $ "Insufficient handlers or missing return clause;"
             <+> "got" <+> pretty idents
             <+> "expected" <+> pretty desired
  let typs = map snd types
  zipWithM_ constrTyp typs (tail typs)
  return (head typs, resE)

generalize :: Env -> Typ -> Scheme
generalize env ty = Scheme (Set.toList vars) ty
  where
    vars = ftv ty `Set.difference` ftv env
