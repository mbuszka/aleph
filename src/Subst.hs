{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Subst where

import Control.Lens
import Control.Monad.Except

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

import Environment
import Error
import Syntax
import Type

type Subst = Map TVar Type

class Substitute a where
  apply :: (MonadError Error m) => Subst -> a -> m a

class FreeVars a where
    ftv :: a -> Set TVar

compose :: (MonadError Error m) => Subst -> Subst -> m Subst
compose a b = M.union a <$> apply a b

instance Substitute Type where
  apply s (TyArr t1 r t2)     = 
    TyArr <$> (apply s t1) <*> (apply s r) <*> (apply s t2)
  apply s v@(TyVar tv)        = return $ M.findWithDefault v tv s
  apply s r@(TyRow _ Nothing) = return r
  apply s r@(TyRow ls (Just v)) = case M.lookup v s of
    Nothing -> return r
    Just (TyVar v')     -> return $ TyRow ls (Just v')
    Just (TyRow ls' v') -> return $ TyRow (ls' ++ ls) v'
    Just t              ->
      throw $ KindError ("tried to substitute " ++ show t ++ "into row")
  apply _ l               = return $ l

instance (Substitute a, Traversable f) => Substitute (f a) where
  apply s = mapM (apply s)

instance FreeVars Type where
  ftv (TyArr t1 r t2)    = ftv t1 `S.union` ftv r `S.union` ftv t2
  ftv (TyRow _ (Just v)) = S.singleton v
  ftv _                  = S.empty
  
instance FreeVars Scheme where
  ftv (Scheme bound ty) = ftv ty `S.difference` S.fromList bound

instance FreeVars a => FreeVars [a] where
  ftv = foldr (S.union . ftv) S.empty

instance FreeVars Environment where
  ftv env = env ^. eTypeContext . to (ftv . M.elems)