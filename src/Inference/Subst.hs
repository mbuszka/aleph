{-# LANGUAGE 
    FlexibleInstances
  , FlexibleContexts
  , OverloadedStrings
#-}

module Inference.Subst 
  ( Subst(..)
  , Substitute(..)
  , FreeVars(..)
  , compose
  , emptySubst
  , Inference.Subst.extend
  ) where

import Control.Lens
import Control.Monad.Except

import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)

import Inference.Env as Env

import Error
import Print
import Syntax

newtype Subst = Subst { unwrap :: Map TyVar (Either Typ Row) }
  deriving Show

instance Pretty (Either Typ Row) where
  pretty (Left  t) = pretty t
  pretty (Right r) = pretty r

instance Pretty Subst where
  pretty = pretty . Map.toList . unwrap

class Substitute a where
  apply :: (MonadError Error m) => Subst -> a -> m a

class FreeVars a where
  ftv :: a -> Set TyVar

emptySubst :: Subst
emptySubst = Subst Map.empty

compose :: (MonadError Error m, MonadIO m) => Subst -> Subst -> m Subst
compose a b = Subst <$> (Map.union (unwrap a) <$> (unwrap <$> apply a b))

extend :: (MonadError Error m, MonadIO m) => TyVar -> (Either Typ Row) -> Subst -> m Subst
extend v t = compose (Subst $ Map.singleton v t)

instance Substitute Typ where
  apply s (TyArr t1 r t2) = TyArr <$> (apply s t1) <*> (apply s r) <*> (apply s t2)
  apply s (TyVar tv)      = case Map.lookup tv $ unwrap s of
    Nothing        -> return (TyVar tv)
    Just (Left t)  -> return t
    Just (Right _) -> throw $ KindError $ "Tried to assign row to a type var" <+> pretty tv
  apply _ l               = return $ l

instance Substitute Row where
  apply s r@(Row ls (Just v)) = case Map.lookup v $ unwrap s of
    Nothing                  -> return r
    Just (Right (Row ls' t)) -> return $ Row (ls ++ ls') t
    Just (Left t)            ->
      throw $ KindError $ "Tried to assign type to row var" <+> pretty v
  apply s r = return r

instance Substitute Subst where
  apply s s' = Subst <$> (apply s $ unwrap s')

instance (Substitute a) => Substitute (Map k a) where
  apply s = mapM (apply s)

instance (Substitute a, Substitute b) => Substitute (Either a b) where
  apply s (Left a)  = Left  <$> apply s a
  apply s (Right b) = Right <$> apply s b

instance Substitute Scheme where
  apply s (Scheme vs t) = Scheme vs <$> apply s t

instance Substitute Env where
  apply s e = 
    let tc = Env._eTypeContext e
    in do
      tc' <- apply s tc
      return $ set Env.eTypeContext tc' e

instance FreeVars Typ where
  ftv (TyArr t1 r t2)    = ftv t1 `Set.union` ftv r `Set.union` ftv t2
  ftv _                  = Set.empty

instance FreeVars Row where
  ftv (Row _ (Just v)) = Set.singleton v
  ftv _                = Set.empty

instance FreeVars Scheme where
  ftv (Scheme bound ty) = ftv ty `Set.difference` Set.fromList bound

instance FreeVars a => FreeVars [a] where
  ftv = foldr (Set.union . ftv) Set.empty

instance FreeVars Env where
  ftv env = env ^. Env.eTypeContext . to (ftv . Map.elems)