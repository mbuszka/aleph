{-# LANGUAGE
    ConstraintKinds
  , FlexibleContexts
  , TemplateHaskell
  , OverloadedStrings
#-}

module Check where

import Control.Monad.Except
import Control.Monad.State hiding (State)
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Lens

import           Data.List((\\))
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as P

import Prelude hiding (lookup)

import Environment
import Error
import Grammar
import Subst
import Print


data Constraint
  = TyConstr Typ Typ
  | RoConstr Row Row
  deriving Show

instance Pretty Constraint where
  pretty (TyConstr a b) = pretty a P.<+> "~" P.<+> pretty b
  pretty (RoConstr a b) = pretty a P.<+> "~" P.<+> pretty b

type Constraints = [Constraint]

instance Substitute Constraint where
  apply s (TyConstr a b) = TyConstr <$> (apply s a) <*> (apply s b)
  apply s (RoConstr a b) = RoConstr <$> (apply s a) <*> (apply s b)

data State = State
  { _sNextVar :: Int
  , _sSubst   :: Subst
  } deriving (Show)

makeLenses ''State

type Check m = 
  ( MonadError Error m
  , MonadReader Environment m
  , MonadState State m
  , MonadWriter Constraints m
  , MonadIO m
  )

initState = State 0 emptySubst

runCheck :: ExceptT Error (RWST Environment Constraints State IO) a -> IO (Either Error a, State, Constraints)
runCheck c = runRWST (runExceptT c) initEnv initState

process :: (Check m, MonadIO m) => Term -> m (Typ, Row)
process t = do
  ((typ, env), cs) <- listen $ infer t
  s   <- gets _sSubst
  sub <- solve s cs
  -- liftIO $ putDocW 80 $ pretty sub P.<> P.line
  t <- apply sub typ
  e <- apply sub env
  return (t, e)

instantiate :: Check m => Scheme -> m Typ
instantiate (Scheme vs ty) = do
  newVs <- mapM (\v -> do
    -- k <- getKind
    -- var <- case k of
    --   KTyp -> freshTyp
    --   KRow  -> freshRow
    var <- freshRow
    return (v, Right var)) vs
  apply (Subst $ M.fromList newVs) ty

lookupEnv :: Check m => Ident -> m Typ
lookupEnv v = lookup v >>= instantiate

fresh :: (MonadState State m) => m TyVar
fresh = do
  i <- gets _sNextVar
  modify (sNextVar %~ (+1))
  return $ TV $ T.pack ("t" ++ show i)

freshTyp :: (MonadState State m) => m Typ
freshTyp = TyVar <$> fresh

freshRow :: (MonadState State m) => m Row
freshRow = Row [] . Just <$> fresh

constrTyp :: Check m => Typ -> Typ -> m ()
constrTyp a b = tell [TyConstr a b]

constrRow :: Check m => Row -> Row -> m ()
constrRow a b = tell [RoConstr a b]

infer :: Check m => Term -> m (Typ, Row)
infer (Var v)       = (,) <$> lookupEnv v <*> freshRow
-- infer (Lit VBool{}) = (,) (TyLit "Bool")  <$> freshTyp
infer (Lit VInt{})  = (,) (TyLit $ TL "Int")  <$> freshRow
infer (Lit VUnit{}) = (,) (TyLit $ TL "Unit") <$> freshRow
infer (Abs v term) = do
  tv <- freshTyp
  (ty, row) <- inEnv v (Scheme [] tv) $ infer term
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
infer (Bind v t1 t2) = do
  (ty1, e1) <- infer t1
  (ty2, e2) <- inEnv v (Scheme [] ty1) $ infer t2
  constrRow e1 e2
  return (ty2, e2)
infer (Let v body exp) = do
  ((ty1, e1), cs) <- listen $ infer body
  s <- gets _sSubst
  s' <- solve s cs
  modify $ sSubst .~ s'
  constrRow e1 (Row [] Nothing)
  env <- ask
  let s = generalize env ty1
  inEnv v s $ infer exp
-- infer (Handle t hs) = do
--   (tyh, eh, lbl) <- inferHandlers hs
--   (ty1, e1) <- infer t
--   fr <- freshRow
--   constr e1 (Row [lbl] (Just fr))
--   constr (TyVar fr) eh
--   constr ty1 tyh
--   return (tyh, eh)
infer (Lift lbl t) = do
  fr <- fresh
  (ty, eff) <- infer t
  constrRow (Row [lbl] (Just fr)) eff
  return (ty, eff)

-- extractLabel :: Handler -> Set Ident
-- extractLabel (Op x _ _ _) = S.singleton x
-- extractLabel (Ret _ _) = S.empty

-- -- TODO We probably should pass type of handled expression,
-- -- to construct type of continuation.

-- inferHandlers :: [Handler] -> Check (Typ, Typ, Ident)
-- inferHandlers hs = do
--   lbls <- mapM lookupEff . S.toList . fold extractLabel S.empty $ hs
--   lbl <- case S.fromList . S.toList $ lbls of
--     [l] -> return l
--     ls  -> throw $ TypeError $ "Handlers had unexpected amount of effects: " ++ show ls

generalize :: Environment -> Typ -> Scheme
generalize env ty = Scheme (S.toList vars) ty
  where
    vars = ftv ty `S.difference` ftv env

type Solve m = (MonadError Error m, MonadState State m, MonadWriter Constraints m, MonadIO m) 

unifyT :: Solve m => Subst -> (Typ, Typ) -> m (Subst, Constraints)
unifyT s (TyVar a, b) = (,) <$> extend a (Left b) s <*> return []
unifyT s (a, TyVar b) = (,) <$> extend b (Left a) s <*> return []
unifyT s (TyLit a, TyLit b) | a == b = return (s, [])
unifyT s (TyArr a1 r1 b1,  TyArr a2 r2 b2) = 
  let cs = [ TyConstr a1 a2
           , RoConstr r1 r2
           , TyConstr b1 b2
           ]
  in do
    tell cs
    return (s, cs)
unifyT s (t1, t2) = throw $ UnificationError $ "cannot unify " ++ show t1 ++ " with " ++ show t2

unifyR :: Solve m => Subst -> (Row, Row) -> m (Subst, Constraints)
unifyR s (Row l1 v1, Row l2 v2) = let
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
          fr <- fresh
          s' <- extend a (Right $ Row extraInL2 (Just fr)) s
          return $ (s', [])
      
      (Nothing, Just a) ->
        unifyR s (Row l2 v2, Row l1 v1)
      
      (Just a, Just b) -> do
        fr <- fresh
        liftIO $ putDocW 80 $ "unifiying" P.<+> pretty (Row l1 v1) P.<+> "and" P.<+> pretty (Row l2 v2) P.<> P.line
        s' <- (extend a (Right $ Row extraInL2 (Just fr)) s >>=
                extend b (Right $ Row extraInL1 (Just fr)))
        liftIO $ putDocW 80 $ "resulting subst:" P.<+> pretty s' P.<> P.line
        return $ (s', [])
    
solve :: Solve m => Subst -> Constraints -> m Subst
solve s (c:cs) = do
  c' <- apply s c
  (s', cs') <- case c' of
    TyConstr a b -> unifyT s (a, b)
    RoConstr a b -> unifyR s (a, b)
  solve s' (cs' ++ cs)
solve s [] = return s