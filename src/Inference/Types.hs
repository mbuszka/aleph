{-# LANGUAGE 
    OverloadedStrings
  , TemplateHaskell
#-}

module Inference.Types where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State  hiding (State)
import Control.Monad.Writer

import qualified Data.Map  as Map
import qualified Data.Text as Text

import Inference.Env   as Env
import Inference.Subst as Subst
import Syntax.Grammar

import Error
import Print

type Check m = 
  ( MonadError Error m
  , MonadReader Env m
  , MonadState State m
  , MonadWriter Constraints m
  , MonadIO m
  )

type Solve m = 
  ( MonadError Error m
  , MonadState State m
  , MonadWriter Constraints m
  , MonadIO m
  ) 

type Constraints = [Constraint]

data Constraint
  = TyConstr Typ Typ
  | RoConstr Row Row
  deriving Show

data State = State
  { _sNextVar :: Int
  , _sSubst   :: Subst
  } deriving (Show)

instance Pretty Constraint where
  pretty (TyConstr a b) = pretty a <+> "~" <+> pretty b
  pretty (RoConstr a b) = pretty a <+> "~" <+> pretty b


instance Substitute Constraint where
  apply s (TyConstr a b) = TyConstr <$> (apply s a) <*> (apply s b)
  apply s (RoConstr a b) = RoConstr <$> (apply s a) <*> (apply s b)

makeLenses ''State

canonicalize :: Check m => Scheme -> m Scheme
canonicalize (Scheme vs t) = 
  Scheme (take (length vs) idents) <$> apply (Subst $ Map.fromList $ zip vs rows) t
  where idents = map (TV . Text.pack . (:[])) ['a' .. 'z']
        rows   = map (Right . Row [] . Just) idents


instantiate :: Check m => Scheme -> m Typ
instantiate (Scheme vs ty) = do
  newVs <- mapM (\v -> do
    var <- freshRow
    return (v, Right var)) vs
  apply (Subst $ Map.fromList newVs) ty

lookupEnv :: Check m => Ident -> m Typ
lookupEnv v = Env.lookup v >>= instantiate

fresh :: (MonadState State m) => m TyVar
fresh = do
  i <- gets _sNextVar
  modify (sNextVar %~ (+1))
  return $ TV $ Text.pack ("t" ++ show i)

freshTyp :: (MonadState State m) => m Typ
freshTyp = TyVar <$> fresh

freshRow :: (MonadState State m) => m Row
freshRow = Row [] . Just <$> fresh

constrTyp :: Check m => Typ -> Typ -> m ()
constrTyp a b = tell [TyConstr a b]

constrRow :: Check m => Row -> Row -> m ()
constrRow a b = tell [RoConstr a b]