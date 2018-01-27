{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , TemplateHaskell
#-}

module Inference.Env where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

import           Data.List as List
import qualified Data.Map  as Map
import           Data.Map (Map)

import Error
import Syntax
import Print

data Scheme = Scheme [TyVar] Typ
  deriving Show

instance Pretty Scheme where
  pretty (Scheme tv t) = "forall" <+> (sep . map pretty $ tv) <> "." <+> pretty t

data Env = Env
  { _eTypeContext :: Map Ident Scheme
  , _eOperations  :: Map Ident (TyLit, Typ, Typ)
  , _eEffects     :: Map TyLit [Ident]
  } deriving (Show)

instance Pretty Env where
  pretty (Env tc ops eff) = 
       "Environment:" <> line
    <> indent 2 (align (vsep (map (\(k, s) -> pretty k <+> ":" <+> pretty s) $ Map.assocs tc))) <> line
    <> "Effects:" <> line
    <> indent 2 (align (vsep (map (\(k, i) -> pretty k <+> "=" <+> pretty i) $ Map.assocs eff))) <> line

makeLenses ''Env

empty :: Env
empty = Env Map.empty Map.empty Map.empty

lookup :: (MonadReader Env m, MonadError Error m) => Ident -> m Scheme
lookup v = do
  ms <- asks $ \e -> e ^. eTypeContext . to (Map.lookup v)
  case ms of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable v

lookupOp :: (MonadReader Env m, MonadError Error m) => Ident -> m (TyLit, Typ, Typ)
lookupOp v = do
  ml <- asks $ \e -> e ^. eOperations . to (Map.lookup v)
  case ml of
    Just l  -> return l
    Nothing -> throwError $ UnknownOperation v

lookupEff :: (MonadReader Env m, MonadError Error m) => TyLit -> m [Ident]
lookupEff v = do
  ml <- asks $ \e -> e ^. eEffects . to (Map.lookup v)
  case ml of
    Just l  -> return l
    Nothing -> throwError $ UnknownEffect v

extended :: (MonadReader Env m) => Ident -> Scheme -> m a -> m a
extended v s = local $ extend v s

extend :: Ident -> Scheme -> Env -> Env
extend id s = eTypeContext %~ Map.insert id s

combine :: Env -> Env -> Env
combine (Env tcs1 ops1 eff1) (Env tcs2 ops2 eff2) =
  Env (Map.union tcs1 tcs2) (Map.union ops1 ops2) (Map.union eff1 eff2)