{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , TemplateHaskell
#-}

module Environment where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

import qualified Data.Map as M
import           Data.Map (Map)

import Error
import Grammar

data Scheme = Scheme [TyVar] Typ
  deriving Show

data Environment = Env
  { _eTypeContext :: Map Ident Scheme
  , _eOperations  :: Map Ident (TyLit, Typ, Typ)
  } deriving (Show)

makeLenses ''Environment

operations :: Map Ident (TyLit, Typ, Typ)
operations = M.fromList
  [ (ID "put",   (TL "ST", tv "Int",  tv "Unit"))
  , (ID "get",   (TL "ST", tv "Unit", tv "Int" ))
  , (ID "print", (TL "IO", tv "Int",  tv "Unit"))
  ]
  where tv = TyVar . TV

effects :: Map Ident Scheme
effects = fmap (\(eff, a, b) -> 
  let v = TV "'a" in
    Scheme [v] (TyArr a (Row [eff] (Just v)) b)) operations

initEnv :: Environment
initEnv = Env effects operations

lookup :: (MonadReader Environment m, MonadError Error m) => Ident -> m Scheme
lookup v = do
  ms <- asks (\env -> env ^. eTypeContext . to (M.lookup v))
  case ms of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable (show v)

lookupEff :: (MonadReader Environment m, MonadError Error m) => Ident -> m (TyLit, Typ, Typ)
lookupEff v = do
  ml <- asks (\e -> e ^. eOperations . to (M.lookup v))
  case ml of
    Just l  -> return l
    Nothing -> throwError $ UnknownOperation v

inEnv :: (MonadReader Environment m) => Ident -> Scheme -> m a -> m a
inEnv v s = local (eTypeContext %~ M.insert v s)
