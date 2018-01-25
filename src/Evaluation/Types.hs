{-# LANGUAGE
    ConstraintKinds
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

type Handlers m = Map Ident ([Cont] -> Fun m)
type Fun m = ExpVal -> Ctx -> m ExpVal


data Ctx = Ctx
  { _contStack :: [Cont]
  }

data Cont
  = HandlerFrame 
      TyLit
      (forall m. MonadEval m => Fun m)
      (forall m. MonadEval m => Handlers m)
  | RegularFrame (forall m. MonadEval m => Fun m)
  | LiftFrame TyLit

data ExpVal
  = IntVal Integer
  | BoolVal Bool
  | ListVal [Integer]
  | UnitVal
  | ResVal [Cont]
  | FunVal (forall m. MonadEval m => Fun m)
  | OpVal Ident TyLit

makeLenses ''Ctx

instance Pretty Cont where
  pretty (HandlerFrame l cs _) = "Handlers of" <+> pretty l
  pretty (RegularFrame _)      = "Regular cont"
  pretty (LiftFrame l)         = "Lift" <+> pretty l

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pretty m = align (vsep (map 
    (\(k, v) -> pretty k <+> "->" <+> pretty v) $ M.assocs m))

instance Pretty Ctx where
  pretty (Ctx cs) = "Runtime stack:" <> line
      <> align (vsep $ map pretty cs) <> line
  

instance Pretty ExpVal where
  pretty (IntVal i)  = pretty i
  pretty (BoolVal True) = "true"
  pretty (BoolVal False) = "false"
  pretty UnitVal     = "()"
  pretty (ResVal  _) = "resumption"
  pretty (FunVal  f) = "function"
  pretty (OpVal i l) = "operator:" <> pretty i <+> pretty l

abort :: (MonadEval m) => Ctx -> (forall a. Doc a) -> m b
abort ctx msg = throw $ RuntimeError $ msg <> line <> pretty ctx

getOrErr :: (MonadEval m, Pretty k, Ord k) => Ctx -> k -> Map k a -> m a
getOrErr c k m = fromMaybe c ("Could not find" <+> pretty k) $ M.lookup k m

fromMaybe :: (MonadEval m) => Ctx -> (forall b. Doc b) -> Maybe a -> m a
fromMaybe c _ (Just v) = return v
fromMaybe c t  Nothing = abort c t

apply :: MonadEval m => Ctx -> ExpVal -> m ExpVal
apply (Ctx [])     v = return v
apply (Ctx (c:cs)) v = case c of
  RegularFrame f     -> f v (Ctx cs)
  HandlerFrame _ r _ -> r v (Ctx cs)
  LiftFrame _        -> apply (Ctx cs) v

pushFrame :: Cont -> Ctx -> Ctx
pushFrame c (Ctx cs) = Ctx $ c:cs

splitOn :: MonadEval m => TyLit -> Ctx -> m ([Cont], Handlers m, [Cont])
splitOn l c@(Ctx cs) = let
  aux :: forall m. MonadEval m => Int -> [Cont] -> m ([Cont], Handlers m, [Cont])
  aux k (f@(HandlerFrame l' r hs) : fs)
    | l == l' && k == 0 = return ([f], hs, fs)
    | l == l' = do
        (front, hs, rest) <- aux (k - 1) fs
        return (f:front, hs, rest)
  aux k (f@(LiftFrame l') : fs)
    | l == l' = do
      (front, hs, rest) <- aux (k + 1) fs
      return (f:front, hs, rest)
  aux k (f:fs) = do
    (front, hs, rest) <- aux k fs
    return (f:front, hs, rest)
  aux _ [] = abort c $ "Could not find handler for" <+> pretty l
  in aux 0 cs
