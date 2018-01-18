{-# LANGUAGE
    GADTs
  , ConstraintKinds
  , FlexibleContexts
  , LambdaCase
  , OverloadedStrings
  , RankNTypes
  , TemplateHaskell
#-}

module Evaluation.Types where

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
import Print
import Syntax.Grammar(Ident, TyLit)

type MonadEval m =
  ( MonadReader Ctx m
  , MonadWriter [ExpVal] m
  , MonadError  Error m
  , MonadIO m
  )

newtype Handlers = Handlers
  { operations :: forall m. MonadEval m 
              => Map Ident (ExpVal -> Cont -> m ExpVal)
  }

instance Pretty Handlers where
  pretty (Handlers ops) = "handlers..."

data Ctx = Ctx
  { _handlers :: Map TyLit [Handlers]
  , _effects  :: Map Ident TyLit
  }

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = align (vsep (map 
    (\(k, v) -> pretty k <+> "->" <+> pretty v) $ M.assocs m))

instance Pretty Ctx where
  pretty (Ctx hs eff) = "Runtime context:" <> line
    <> "Handlers:" <+> pretty hs <> line
    <> "Effects:" <+> pretty eff <> line

newtype Cont = Cont { apply :: forall m. (MonadEval m) => ExpVal -> m ExpVal }

data ExpVal
  = IntVal Integer
  | UnitVal
  | ContVal Cont
  | FunVal (forall m. MonadEval m => ExpVal -> Cont -> m ExpVal)
  | OpVal Ident TyLit

instance Pretty ExpVal where
  pretty (IntVal i)  = pretty i
  pretty UnitVal     = "()"
  pretty (ContVal c) = "continuation"
  pretty (FunVal  f) = "function"
  pretty (OpVal i l) = "operator:" <> pretty i <+> pretty l

makeLenses ''Ctx
makeLenses ''Handlers

abort :: (MonadEval m) => (forall a. Doc a) -> m b
abort msg = do
  e <- ask
  throw $ RuntimeError $ msg <> line <> pretty e

getOrErr :: (MonadEval m, Pretty k, Ord k) => k -> Map k a -> m a
getOrErr k m = case M.lookup k m of
  Just v  -> return v
  Nothing -> abort $ "Could not find" <+> pretty k

lookupLbl :: (MonadEval m) => Ident -> m TyLit
lookupLbl x = do
  e <- ask
  e ^. effects . to (getOrErr x)

lookupHandler :: (MonadEval m) 
              => TyLit -> Ident -> m (ExpVal -> Cont -> m ExpVal)
lookupHandler lbl id = do
  e  <- ask
  hs <- e ^. handlers . to (getOrErr lbl)
  getOrErr id . operations . head $ hs

withHandlers :: MonadEval m => TyLit -> Handlers -> m a -> m a
withHandlers lbl hs = local $ over handlers (M.insertWith (++) lbl [hs])

withoutHandlers :: MonadEval m => TyLit -> m a -> m a
withoutHandlers l = local $ over handlers (M.adjust tail l)