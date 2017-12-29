{-# LANGUAGE FlexibleContexts #-}

module Renamer where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS

import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import           Data.Bifunctor

-- import Data.Generics

import NameSupply
import Syntax
import Common

type RenameMonad = ExceptT String (RWS (Map String String) (Map String String) NameStore)

initialMap :: [TopLevel] -> Map String String
initialMap ts = M.fromList $ foldr (\x acc -> 
  case x of
    Definition b _ -> let n = getName b in (n, n) : acc
    Runnable _ -> acc) [] ts

renameAll :: [TopLevel] -> Either Error ([TopLevel], NameStore, Map String String)
renameAll ts =
  let init = initialMap ts
      (e, s, w) = runRWS (runExceptT $ mapM rename ts) init (NameStore 0 0)
  in bimap RenameError (\x -> (x, s, w)) e

withRename :: (HasName a, HasName b, MonadReader (Map String String) m) => a -> b -> m c -> m c
withRename f t = local (M.insert (getName f) (getName t))

class HasName a where
  getName :: a -> Identifier

instance HasName TypeBinding where
  getName (TBind id _) = id

instance HasName VarBinding where
  getName (VBind id _) = id

instance HasName Binding where
  getName (BVar v) = getName v
  getName (BTyp v) = getName v

class Rename a where
  rename :: a -> RenameMonad a

instance Rename TypeBinding where
  rename (TBind oldId kind) = do
    ty <- freshTypeName
    tell $ M.singleton ty oldId
    return $ TBind ty kind

instance Rename VarBinding where
  rename (VBind oldId ty) = do
    var <- freshVarName
    tell $ M.singleton var oldId
    newTy <- maybe (return Nothing) (fmap Just . rename) ty
    return $ VBind var newTy

instance Rename Binding where
  rename (BVar v) = BVar <$> rename v
  rename (BTyp v) = BTyp <$> rename v

instance Rename TopLevel where
  rename (Definition id val) = Definition id <$> rename val
  rename (Runnable exp) = Runnable <$> rename exp

instance Rename Expression where
  rename (App e1 e2) = App <$> rename e1 <*> rename e2
  rename (RowExp row) = RowExp <$> rename row
  rename (Val v) = Val <$> rename v
  rename (Lift id exp) = Lift id <$> rename exp
  rename (HandleExp exp hs) = HandleExp <$> rename exp <*> mapM rename hs
  rename (LetExp bind bound body) = do
    newBind <- rename bind
    newBound <- rename bound
    newBody <- withRename bind newBind (rename body)
    return $ LetExp newBind newBound newBody

instance Rename Value where
  rename (Lambda bind exp) = do
    newBind <- rename bind
    newExp <- withRename bind newBind $ rename exp
    return $ Lambda newBind newExp
  rename (Variable id) = do
    newId <- asks (M.lookup id)
    case newId of 
      Nothing -> throwError $ "Unbound variable: " ++ id
      Just id -> return $ Variable id
  rename (Lit l) = return $ Lit l

instance Rename Type where
  rename (TyArrow t1 row t2) = TyArrow <$> rename t1 <*> rename row <*> rename t2
  rename (TyForall bind ty) = do
    newBind <- rename bind
    newType <- withRename bind newBind $ rename ty
    return $ TyForall newBind newType
  rename ty = return ty

instance Rename Row where
  rename (ROpen lbls id) = do
    newId <- asks (M.lookup id)
    case newId of
      Nothing -> throwError $ "Unbound variable: " ++ id
      Just id -> return $ ROpen lbls id
  rename row = return row

instance Rename Handler where
  rename (Handler op bind cont exp) = do
    newBind <- rename bind
    newCont <- freshVarName
    newExp <- local (M.insert cont newCont) $ withRename bind newBind $ rename exp
    return $ Handler op newBind newCont newExp
