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
import Print

data Handlers = Handlers
  { _operations :: forall m. EvalMonad m => Map Ident (ExpVal -> Cont -> m ExpVal)
  -- , _returnOp   :: (Ident, Term)
  -- , _hCont      :: Cont
  }

instance Pretty Handlers where
  pretty (Handlers ops) = "handlers..."

data Environment = Env
  { _values   :: Map Ident ExpVal
  , _handlers :: Map TyLit [Handlers]
  , _effects  :: Map Ident TyLit
  }

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = align (vsep (map (\(k, v) -> pretty k <+> "->" <+> pretty v) $ M.assocs m))

instance Pretty Environment where
  pretty (Env vs hs eff) = "Runtime environment:" <> line
    <> "Values:" <+> pretty vs <> line
    <> "Handlers:" <+> pretty hs <> line
    <> "Effects:" <+> pretty eff <> line

type EvalMonad m =
  ( MonadReader Environment m
  , MonadWriter [ExpVal] m
  , MonadError  Error m
  , MonadIO m
  )

newtype Cont = Cont { apply :: forall m. (EvalMonad m) => ExpVal -> m ExpVal }

data ExpVal
  = IntVal Integer
  | UnitVal
  | ContVal Cont
  | FunVal (forall m. EvalMonad m => ExpVal -> Cont -> m ExpVal)
  | OpVal Ident TyLit

instance Pretty ExpVal where
  pretty (IntVal i)  = pretty i
  pretty UnitVal     = "()"
  pretty (ContVal c) = "continuation"
  pretty (FunVal  f) = "function"
  pretty (OpVal i l) = "operator:" <> pretty i <+> pretty l

makeLenses ''Environment
makeLenses ''Handlers

runEval :: (MonadError Error m, MonadIO m) 
  => Environment -> ReaderT Environment (WriterT [ExpVal] m) a -> m (a, [ExpVal])
runEval e c = runWriterT (runReaderT c e)

defaultHandlers :: Map TyLit [Handlers]
defaultHandlers = M.fromList
  [ (TL "IO", [ Handlers 
      (M.fromList 
        [ (ID "print", \v k -> tell [v] >> apply k UnitVal)
        ])])
  ]

initialEnv :: Map Ident TyLit -> Environment
initialEnv = Env M.empty defaultHandlers 

evalProgram :: EvalMonad m => [Top] -> m ()
evalProgram (Def x t : ts) = do
  v <- eval t finalCont
  local (extendEnv x v) (evalProgram ts)
evalProgram (Run t : ts) = eval t finalCont >> evalProgram ts
evalProgram (EffDef{} : ts) = evalProgram ts
evalProgram [] = return ()

finalCont :: Cont
finalCont = Cont $ \v -> return v

lookupEnv :: EvalMonad m => Ident -> m ExpVal
lookupEnv v = do
  mv <- asks (\e -> e ^. values . to (M.lookup v))
  case mv of
    Just val -> return val
    Nothing  -> do
      mo <- asks (\e -> e ^. effects . at v)
      case mo of
        Just lbl -> return $ OpVal v lbl
        Nothing  ->  abort $ "Unbound variable:" <+> pretty v

extendEnv :: Ident -> ExpVal -> Environment -> Environment
extendEnv x v = over values (M.insert x v)

withHandlers :: EvalMonad m => TyLit -> Handlers -> m a -> m a
withHandlers lbl hs = local $ over handlers (M.insertWith (++) lbl [hs])

find p (x:xs) = case p x of
  Just y  -> Just y
  Nothing -> find p xs

prepareHandlers :: Cont -> [Handler] -> (Handlers, Cont)
prepareHandlers k hs =
  let Just (x, t) = 
        find (\case Ret x t -> Just (x, t)
                    _ -> Nothing) hs
      k' = Cont (\v -> local (extendEnv x v) $ eval t k)
  in  (Handlers 
        (M.fromList $ foldr (\x acc -> case x of
          Op id a cont t -> 
            (id, \v k -> local (extendEnv a v . extendEnv cont (ContVal k)) $ eval t k') : acc
          Ret _ _ -> acc) [] hs)
      , k')

eval :: EvalMonad m => Term -> Cont -> m ExpVal
eval t k = case t of
  Var v -> lookupEnv v >>= apply k
  Lit v -> case v of
    Grammar.VInt i -> apply k $ IntVal i
    Grammar.VUnit  -> apply k UnitVal
  Abs id exp -> do
    vs <- asks _values
    apply k $ FunVal (\v k ->
      -- liftIO $ putDocW 80 $ "Extending env with:" <+> pretty id <+> "->" <+> pretty v <> line
      local (set values (M.insert id v vs)) $ do
        vals <- asks _values
        liftIO $ putDocW 80 $ "Values:" <+> pretty vals <> line
        eval exp k)
  App fun exp ->
    eval fun $ Cont $ \case 
      FunVal f   -> eval exp $ Cont (`f` k)
      OpVal id _ -> eval exp $ Cont (\v -> applyHandler id v k)
      ContVal k  -> eval exp k
      v -> abort $ "Unexpected value:" <+> pretty v <+> "function required."
  Let id body exp ->
    eval body $ Cont $ \v ->
      local (extendEnv id v) $ eval exp k
  Bind id e1 e2 ->
    eval e1 $ Cont $ \v ->
      local (extendEnv id v) $ eval e2 k
  Handle lbl exp hs ->
    let (hs', k') = prepareHandlers k hs
    in  withHandlers lbl hs' $ eval exp k'

getErr :: (EvalMonad m, Pretty k, Ord k) => k -> Map k a -> m a
getErr k m = case M.lookup k m of
  Just v  -> return v
  Nothing -> abort $ "Could not find" <+> pretty k

abort :: (EvalMonad m) => (forall a. Doc a) -> m b
abort msg = do
  e <- ask
  throw $ RuntimeError $ msg <> line <> pretty e

applyHandler :: EvalMonad m => Ident -> ExpVal -> Cont -> m ExpVal
applyHandler id v k = do
  e <- ask
  lbl <- e ^. effects . to (getErr id)
  hs  <- e ^. handlers . to (getErr lbl)
  let h = head hs ^. operations . to (M.! id)
  h v k

