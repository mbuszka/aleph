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
import Control.Monad.Writer hiding ((<>))
import Control.Monad.Except

import qualified Data.List as List
import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import           Data.Text(Text)

import Error
import Print
import Syntax.Grammar(Ident, TyLit)

type MonadEval m =
  ( MonadWriter [ExpVal] m
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
  }

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = align (vsep (map 
    (\(k, v) -> pretty k <+> "->" <+> pretty v) $ M.assocs m))

instance Pretty Ctx where
  pretty (Ctx hs) = "Runtime context:" <> line
    <> "Handlers:" <+> pretty hs <> line
    -- <> "Effects:" <+> pretty eff <> line

newtype Cont = Cont { apply :: forall m. (MonadEval m) => ExpVal -> m ExpVal }

data ExpVal
  = IntVal Integer
  | BoolVal Bool
  | ListVal [Integer]
  | UnitVal
  | ContVal Cont
  | FunVal (forall m. MonadEval m => ExpVal -> Ctx -> Cont -> m ExpVal)
  | OpVal Ident TyLit

instance Pretty ExpVal where
  pretty (IntVal i)  = pretty i
  pretty (BoolVal True) = "true"
  pretty (BoolVal False) = "false"
  pretty UnitVal     = "()"
  pretty (ContVal c) = "continuation"
  pretty (FunVal  f) = "function"
  pretty (OpVal i l) = "operator:" <> pretty i <+> pretty l

makeLenses ''Ctx
makeLenses ''Handlers

abort :: (MonadEval m) => Ctx -> (forall a. Doc a) -> m b
abort ctx msg = throw $ RuntimeError $ msg <> line <> pretty ctx

getOrErr :: (MonadEval m, Pretty k, Ord k) => Ctx -> k -> Map k a -> m a
getOrErr c k m = fromMaybe c ("Could not find" <+> pretty k) $ M.lookup k m

fromMaybe :: (MonadEval m) => Ctx -> (forall b. Doc b) -> Maybe a -> m a
fromMaybe c _ (Just v) = return v
fromMaybe c t  Nothing = abort c t

lookupHandler :: (MonadEval m) 
              => TyLit -> Ident -> Ctx -> m (ExpVal -> Cont -> m ExpVal)
lookupHandler lbl id ctx = do
  hs <- ctx ^. handlers . to (getOrErr ctx lbl)
  h <-  fromMaybe ctx ("No handler for:" <+> pretty lbl) $ Maybe.listToMaybe hs
  getOrErr ctx id $ operations h

insertHandlers :: TyLit -> Handlers -> Ctx -> Ctx
insertHandlers lbl hs = over handlers (M.insertWith (++) lbl [hs])

removeHandlers :: TyLit -> Ctx -> Ctx
removeHandlers lbl = over handlers (M.adjust (drop 1) lbl)