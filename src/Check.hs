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
import Control.Monad
import Control.Lens

import           Data.List((\\), sort)
import qualified Data.Map as M
import           Data.Map(Map)
import           Data.Maybe(catMaybes, isNothing)
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
infer (Handle lbl t hs) = do
  (ty1, e1) <- infer t
  fr <- fresh
  constrRow e1 (Row [lbl] (Just fr))
  (tyR, eR) <- inferHandlers lbl (ty1, (Row [] (Just fr))) hs
  return (tyR, eR)
infer (Lift lbl t) = do
  fr <- fresh
  (ty, eff) <- infer t
  constrRow (Row [lbl] (Just fr)) eff
  return (ty, eff)

inferHandler :: Check m => TyLit -> (Typ, Row) -> Handler -> m (Maybe Ident, Typ)
inferHandler hLbl (resT, resE) (Op id arg cont exp) = do
  (lbl, argT, retT) <- lookupOp id
  when (hLbl /= lbl) $ throw $ 
    TypeError $ "Could not match operation's effect" <+> pretty lbl
             <+> "with handler's effect" <+> pretty hLbl
  (ty, env) <- inEnv arg (Scheme [] argT) 
               $ inEnv cont (Scheme [] (TyArr retT resE resT)) $ infer exp
  constrRow env resE
  return (Just id, ty)
inferHandler hLbl (resT, resE) (Ret val exp) = do
  (ty, env) <- inEnv val (Scheme [] resT) $ infer exp
  constrRow env resE
  return (Nothing, ty)
  

-- -- TODO We probably should pass type of handled expression,
-- -- to construct type of continuation.

inferHandlers :: Check m => TyLit -> (Typ, Row) -> [Handler] -> m (Typ, Row)
inferHandlers lbl (resT, resE) hs = do
  types <- mapM (inferHandler lbl (resT, resE)) hs
  desired <- lookupEff lbl
  -- liftIO $ putDocW 80 $ pretty desired
  let idents = catMaybes . map fst $ types
  let ret    = filter isNothing . map fst $ types
  when (sort idents /= desired || ret /= [Nothing]) $ throw $
    TypeError $ "Insufficient handlers or missing return clause;"
             <+> "got" <+> pretty idents
             <+> "expected" <+> pretty desired
  let typs = map snd types
  zipWithM constrTyp typs (tail typs)
  return (head typs, resE)

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
unifyT s (t1, t2) = throw $ UnificationError $ pretty t1 <+> "and" <+> pretty t2

unifyR :: Solve m => Subst -> (Row, Row) -> m (Subst, Constraints)
unifyR s (r1@(Row l1 v1), r2@(Row l2 v2)) = let
  extraInL1 = l1 \\ l2
  extraInL2 = l2 \\ l1 in
    case (v1, v2) of
      (Nothing, Nothing) -> 
        if l1 /= l2 
        then throw $ UnificationError $ pretty r1 <+> "and" <+> pretty r2
        else return (s, [])
      
      (Just a, Nothing) ->
        if extraInL1 /= []
        then throw $ UnificationError $ pretty r1 <+> "and" <+> pretty r2
        else do
          fr <- fresh
          s' <- extend a (Right $ Row extraInL2 (Just fr)) s
          return $ (s', [])
      
      (Nothing, Just a) ->
        unifyR s (Row l2 v2, Row l1 v1)
      
      (Just a, Just b) -> do
        fr <- fresh
        -- liftIO $ putDocW 80 $ "unifiying" P.<+> pretty (Row l1 v1) P.<+> "and" P.<+> pretty (Row l2 v2) P.<> P.line
        s' <- (extend a (Right $ Row extraInL2 (Just fr)) s >>=
                extend b (Right $ Row extraInL1 (Just fr)))
        -- liftIO $ putDocW 80 $ "resulting subst:" P.<+> pretty s' P.<> P.line
        return $ (s', [])
    
solve :: Solve m => Subst -> Constraints -> m Subst
solve s (c:cs) = do
  c' <- apply s c
  (s', cs') <- case c' of
    TyConstr a b -> unifyT s (a, b)
    RoConstr a b -> unifyR s (a, b)
  solve s' (cs' ++ cs)
solve s [] = return s