{-# LANGUAGE 
    FlexibleInstances
  , FlexibleContexts
  , OverloadedStrings
#-}

module Subst 
  ( Subst(..)
  , Substitute(..)
  , FreeVars(..)
  , compose
  , emptySubst
  , extend
  ) where

import Control.Lens
import Control.Monad.Except

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.Set as S
import           Data.Set (Set)

import Environment
import Error
import Grammar
import Print

newtype Subst = Subst { unwrap :: Map TyVar (Either Typ Row) }
  deriving Show

instance Pretty (Either Typ Row) where
  pretty (Left  t) = pretty t
  pretty (Right r) = pretty r

instance Pretty Subst where
  pretty = pretty . M.toList . unwrap

class Substitute a where
  apply :: (MonadError Error m) => Subst -> a -> m a

class FreeVars a where
  ftv :: a -> Set TyVar

emptySubst :: Subst
emptySubst = Subst M.empty

compose :: (MonadError Error m, MonadIO m) => Subst -> Subst -> m Subst
compose a b = do
  -- liftIO $ putDocW 80 $ "extending subst with" <+> pretty a <> line
  Subst <$> (M.union (unwrap a) <$> (unwrap <$> apply a b))

extend :: (MonadError Error m, MonadIO m) => TyVar -> (Either Typ Row) -> Subst -> m Subst
extend v t s = compose (Subst $ M.singleton v t) s

instance Substitute Typ where
  apply s (TyArr t1 r t2) = TyArr <$> (apply s t1) <*> (apply s r) <*> (apply s t2)
  apply s (TyVar tv)      = case M.lookup tv $ unwrap s of
    Nothing        -> return (TyVar tv)
    Just (Left t)  -> return t
    Just (Right _) -> throw $ KindError $ "Tried to assign row to a type var" <+> pretty tv
  apply _ l               = return $ l

instance Substitute Row where
  apply s r@(Row ls (Just v)) = case M.lookup v $ unwrap s of
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

instance FreeVars Typ where
  ftv (TyArr t1 r t2)    = ftv t1 `S.union` ftv r `S.union` ftv t2
  ftv _                  = S.empty

instance FreeVars Row where
  ftv (Row _ (Just v)) = S.singleton v
  ftv _                = S.empty

instance FreeVars Scheme where
  ftv (Scheme bound ty) = ftv ty `S.difference` S.fromList bound

instance FreeVars a => FreeVars [a] where
  ftv = foldr (S.union . ftv) S.empty

instance FreeVars Environment where
  ftv env = env ^. eTypeContext . to (ftv . M.elems)