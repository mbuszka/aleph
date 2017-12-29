{-# LANGUAGE FlexibleContexts #-}

module NameSupply where

import Control.Monad.State

data NameStore = NameStore
  { varNameState :: Int
  , typeNameState :: Int
  } deriving Show

freshVarName :: (MonadState NameStore m) => m String
freshVarName = do
  i <- gets varNameState
  modify (\s -> s { varNameState = varNameState s + 1})
  return $ "_v" ++ show i

freshTypeName :: (MonadState NameStore m) => m String
freshTypeName = do
  i <- gets typeNameState
  modify (\s -> s { typeNameState = typeNameState s + 1})
  return $ "'t" ++ show i