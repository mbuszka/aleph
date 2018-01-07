{-# LANGUAGE
    FlexibleContexts
  , TemplateHaskell
  , QuasiQuotes
#-}

module Check where

import Control.Monad.Except
import Control.Monad.State hiding (State)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Lens

import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import Data.List((\\))
-- import Data.Maybe(fromMaybe)

import Prelude hiding (lookup)

import Environment
import Error
import Subst
import Grammar

data State = State
  { _sNextVar :: Int
  , _sSubst   :: Subst
  } deriving (Show)

makeLenses ''State

type Constraint = (Typ, Typ)
type Constraints = [Constraint]

type Check a = ExceptT Error (RWS Environment Constraints State) a

effects :: Map Ident Scheme
effects = M.fromList
  [ (Ident "put",   Scheme [TVar "'a"] [typ| Int -> [ST | 'a] Unit |])
  , (Ident "print", Scheme [TVar "'a"] [typ| Int -> [IO | 'a] Unit |])
  ]

initState = State 0 M.empty
initEnv = Env effects

runCheck :: Check a -> (Either Error a, Constraints)
runCheck c = evalRWS (runExceptT c) initEnv initState

process :: Term -> Check Typ
process t = do
  ((typ, env), cs) <- listen $ infer t
  sub <- solve M.empty cs
  apply sub typ

instantiate :: Scheme -> Check Typ
instantiate (Scheme vs ty) = do
  newVs <- mapM (\v -> do
    -- k <- getKind
    -- var <- case k of
    --   KTyp -> freshTyp
    --   KRow  -> freshRow
    var <- freshTyp
    return (v, var)) vs
  apply (M.fromList newVs) ty

lookupEnv :: Ident -> Check Typ
lookupEnv v = lookup v >>= instantiate

fresh :: (MonadState State m) => m TVar
fresh = do
  i <- gets _sNextVar
  modify (sNextVar %~ (+1))
  return $ TVar ("'t" ++ show i)

freshTyp :: (MonadState State m) => m Typ
freshTyp = TyVar <$> fresh

freshRow :: (MonadState State m) => m TVar
freshRow = fresh

constr :: Typ -> Typ -> Check ()
constr a b = tell [(a, b)]

infer :: Term -> Check (Typ, Typ)
infer (Var v)       = (,) <$> lookupEnv v <*> freshTyp
-- infer (Lit VBool{}) = (,) (TyLit "Bool")  <$> freshTyp
infer (Lit VInt{})  = (,) (TyLit $ Ident "Int")  <$> freshTyp
infer (Lit VUnit{}) = (,) (TyLit $ Ident "Unit") <$> freshTyp
infer (Abs v term) = do
  tv <- freshTyp
  (ty, row) <- inEnv v (Scheme [] tv) $ infer term
  rv <- freshTyp
  return (TyArr tv row ty, rv)
infer (App t1 t2) = do
  (ty1, e1) <- infer t1
  (ty2, e2) <- infer t2
  tv <- freshTyp
  tr <- freshTyp
  constr ty1 (TyArr ty2 tr tv)
  constr e1 e2
  constr tr e1
  return (tv, tr)
infer (Let v body exp) = do
  ((ty1, e1), cs) <- listen $ infer body
  s <- gets _sSubst
  s' <- solve s cs
  modify $ sSubst .~ s'
  constr e1 (TyRow [] Closed)
  env <- ask
  let s = generalize env ty1
  inEnv v s $ infer exp




generalize :: Environment -> Typ -> Scheme
generalize env ty = Scheme (S.toList vars) ty
  where
    vars = ftv ty `S.difference` ftv env



unify :: (MonadError Error m, MonadState State m) => Subst -> (Typ, Typ) -> m (Subst, Constraints)
unify s (TyVar a, b) = (,) <$> compose (M.singleton a b) s <*> return []
unify s (a, TyVar b) = (,) <$> compose (M.singleton b a) s <*> return []
unify s (TyLit a, TyLit b) | a == b = return (s, [])
unify s (TyArr a1 r1 b1,  TyArr a2 r2 b2) = return (s, [(a1, a2), (r1, r2), (b1, b2)])
unify s (TyRow l1 v1, TyRow l2 v2) = unifyRows s l1 l2 v1 v2
unify s (t1, t2) = throw $ UnificationError $ "cannot unify " ++ show t1 ++ " with " ++ show t2

unifyRows :: (MonadError Error m, MonadState State m) =>
              Subst -> [Ident] -> [Ident] -> Row -> Row -> m (Subst, Constraints)
unifyRows s l1 l2 v1 v2 = let 
  extraInL1 = l1 \\ l2
  extraInL2 = l2 \\ l1 in
    case (v1, v2) of
      (Closed, Closed) -> 
        if l1 /= l2 
        then throw $ UnificationError "Cannot unify two closed rows"
        else return (s, [])
      
      (Open a, Closed) ->
        if extraInL1 /= []
        then throw $ UnificationError "Row is closed, but needs extending"
        else do
          fr <- freshRow
          s' <- compose (M.singleton a (TyRow extraInL2 (Open fr))) s
          return $ (s', [])
      
      (Closed, Open a) ->
        unifyRows s l2 l1 v2 v1
      
      (Open a, Open b) -> do
        fr <- freshRow
        s' <- compose (M.fromList [(a, TyRow extraInL2 (Open fr)),
                                   (b, TyRow extraInL1 (Open fr))]) s
        return $ (s', [])
    
solve :: (MonadError Error m, MonadState State m) => Subst -> Constraints -> m Subst
solve s ((a, b):cs) = do
  t1 <- apply s a
  t2 <- apply s b
  (s', cs') <- unify s (t1, t2)
  solve s' (cs' ++ cs)
solve s [] = return s