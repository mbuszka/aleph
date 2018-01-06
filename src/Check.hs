{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Syntax
import Type

data State = State
  { _sNextVar :: Int
  , _sSubst   :: Subst
  } deriving (Show)

makeLenses ''State

type Constraints = [Constraint]

type Check a = ExceptT Error (RWS Environment Constraints State) a

effects :: Map Var Scheme
effects = M.fromList
  [ (V "put",
      Scheme [ TV "'a" ] 
        (TyArr (TyLit "Int") (TyRow [ "State"] (Just (TV "'a"))) (TyLit "()")))
  , (V "print",
      Scheme [ TV "'a" ]
        (TyArr (TyLit "Int") (TyRow [ "IO"] (Just (TV "'a"))) (TyLit "()")))
  ]

initState = State 0 M.empty
initEnv = Env effects

runCheck :: Check a -> (Either Error a, Constraints)
runCheck c = evalRWS (runExceptT c) initEnv initState

process :: Term -> Check Type
process t = do
  ((typ, env), cs) <- listen $ infer t
  sub <- solve M.empty cs
  apply sub typ

instantiate :: Scheme -> Check Type
instantiate (Scheme vs ty) = do
  newVs <- mapM (\v -> do
    -- k <- getKind
    -- var <- case k of
    --   KType -> freshType
    --   KRow  -> freshRow
    var <- freshType
    return (v, var)) vs
  apply (M.fromList newVs) ty

lookupEnv :: Var -> Check Type
lookupEnv v = lookup v >>= instantiate

fresh :: (MonadState State m) => m TVar
fresh = do
  i <- gets _sNextVar
  modify (sNextVar %~ (+1))
  return $ TV ("'t" ++ show i)

freshType :: (MonadState State m) => m Type
freshType = TyVar <$> fresh

freshRow :: (MonadState State m) => m TVar
freshRow = fresh

constr :: Type -> Type -> Check ()
constr a b = tell [(a, b)]

infer :: Term -> Check (Type, Type)
infer (Var v)       = (,) <$> lookupEnv v <*> freshType
infer (Lit VBool{}) = (,) (TyLit "Bool")  <$> freshType
infer (Lit VInt{})  = (,) (TyLit "Int")   <$> freshType
infer (Lit VUnit{}) = (,) (TyLit "()")    <$> freshType
infer (Abs (Bind v _) term) = do
  tv <- freshType
  (ty, row) <- inEnv v (Scheme [] tv) $ infer term
  rv <- freshType
  return (TyArr tv row ty, rv)
infer (App t1 t2) = do
  (ty1, e1) <- infer t1
  (ty2, e2) <- infer t2
  tv <- freshType
  tr <- freshType
  constr ty1 (TyArr ty2 tr tv)
  constr e1 e2
  constr tr e1
  return (tv, tr)
infer (Let (Bind v _) body exp) = do
  ((ty1, e1), cs) <- listen $ infer body
  s <- gets _sSubst
  s' <- solve s cs
  modify $ sSubst .~ s'
  constr e1 (TyRow [] Nothing)
  env <- ask
  let s = generalize env ty1
  inEnv v s $ infer exp




generalize :: Environment -> Type -> Scheme
generalize env ty = Scheme (S.toList vars) ty
  where
    vars = ftv ty `S.difference` ftv env



unify :: (MonadError Error m, MonadState State m) => Subst -> (Type, Type) -> m (Subst, Constraints)
unify s (TyVar a, b) = (,) <$> compose (M.singleton a b) s <*> return []
unify s (a, TyVar b) = (,) <$> compose (M.singleton b a) s <*> return []
unify s (TyLit a, TyLit b) | a == b = return (s, [])
unify s (TyArr a1 r1 b1,  TyArr a2 r2 b2) = return (s, [(a1, a2), (r1, r2), (b1, b2)])
unify s (TyRow l1 v1, TyRow l2 v2) = unifyRows s l1 l2 v1 v2
unify s (t1, t2) = throw $ UnificationError $ "cannot unify " ++ show t1 ++ " with " ++ show t2

unifyRows :: (MonadError Error m, MonadState State m) =>
              Subst -> [String] -> [String] -> Maybe TVar -> Maybe TVar -> m (Subst, Constraints)
unifyRows s l1 l2 v1 v2 = let 
  extraInL1 = l1 \\ l2
  extraInL2 = l2 \\ l1 in
    case (v1, v2) of
      (Nothing, Nothing) -> 
        if l1 /= l2 
        then throw $ UnificationError "Cannot unify two closed rows"
        else return (s, [])
      
      (Just a, Nothing) ->
        if extraInL1 /= []
        then throw $ UnificationError "Row is closed, but needs extending"
        else do
          fr <- freshRow
          s' <- compose (M.singleton a (TyRow extraInL2 (Just fr))) s
          return $ (s', [])
      
      (Nothing, Just a) ->
        unifyRows s l2 l1 v2 v1
      
      (Just a, Just b) -> do
        fr <- freshRow
        s' <- compose (M.fromList [(a, TyRow extraInL2 (Just fr)),
                                   (b, TyRow extraInL1 (Just fr))]) s
        return $ (s', [])
    
solve :: (MonadError Error m, MonadState State m) => Subst -> Constraints -> m Subst
solve s ((a, b):cs) = do
  t1 <- apply s a
  t2 <- apply s b
  (s', cs') <- unify s (t1, t2)
  solve s' (cs' ++ cs)
solve s [] = return s