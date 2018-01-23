{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , TemplateHaskell
#-}

module Inference.Environment where

import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens

import           Data.List(sort)
import qualified Data.Map as M
import           Data.Map (Map)

import Error
import Syntax.Grammar
import Print

data Scheme = Scheme [TyVar] Typ
  deriving Show

instance Pretty Scheme where
  pretty (Scheme tv t) = "forall" <+> (sep . map pretty $ tv) <> "." <+> pretty t

data Environment = Env
  { _eTypeContext :: Map Ident Scheme
  , _eOperations  :: Map Ident (TyLit, Typ, Typ)
  , _eEffects     :: Map TyLit [Ident]
  } deriving (Show)

instance Pretty Environment where
  pretty (Env tc ops eff) = 
    "Environment:" <> line
    <> align (vsep (map (\(k, s) -> pretty k <+> ":" <+> pretty s) $ M.assocs tc)) <> line
    <> "Operations:" <> line
    <> align (vsep (map (\(k, (tl, f, t)) -> pretty k <+> "->" <+> pretty tl) $ M.assocs ops)) <> line

makeLenses ''Environment

operations :: Map Ident (TyLit, Typ, Typ)
operations = M.fromList
  [ (ID "put",   (TL "ST", tl "Int",  tl "Unit"))
  , (ID "get",   (TL "ST", tl "Unit", tl "Int" ))
  , (ID "print", (TL "IO", tl "Int",  tl "Unit"))
  ]
  where tl = TyLit . TL

effToOps :: Map TyLit [Ident]
effToOps = fmap sort .
           M.fromListWith (++) .
           map (\(id, l) -> (l, [id])) . M.toList .
           fmap (\(l, _, _) -> l) $ operations

effects :: Map Ident Scheme
effects = fmap (\(eff, a, b) ->
  let v = TV "a" in
    Scheme [v] (TyArr a (Row [eff] (Just v)) b)) operations

prims :: Map Ident Scheme
prims = M.fromList
  [ (ID "add", iii)
  , (ID "sub", iii)
  , (ID "mul", iii)
  , (ID "div", iii)
  , (ID "isZero", ib)
  , (ID "nil", Scheme [] list)
  , (ID "cons", Scheme [TV "a", TV "b"] $ TyArr int (row "a") (TyArr list (row "b") list))
  , (ID "null", Scheme [TV "a"] $ TyArr list (row "a") bool)
  , (ID "head", Scheme [TV "a"] $ TyArr list (row "a") int)
  , (ID "tail", Scheme [TV "a"] $ TyArr list (row "a") list) 
  ] where
      int = TyLit $ TL "Int"
      list = TyLit $ TL "List"
      bool = TyLit $ TL "Bool"
      row = Row [] . Just . TV
      iii = Scheme [TV "a", TV "b"] $ TyArr int (row "a") (TyArr int (row "b") int)
      ib  = Scheme [TV "a"] $ TyArr int (row "a") bool

initEnv :: Environment
initEnv = Env (M.union effects prims) operations effToOps

emptyEnv :: Environment
emptyEnv = Env M.empty M.empty M.empty

lookup :: (MonadReader Environment m, MonadError Error m) => Ident -> m Scheme
lookup v = do
  ms <- asks (\env -> env ^. eTypeContext . to (M.lookup v))
  case ms of
    Just t -> return t
    Nothing -> throwError $ UnboundVariable v

lookupOp :: (MonadReader Environment m, MonadError Error m) => Ident -> m (TyLit, Typ, Typ)
lookupOp v = do
  ml <- asks (\e -> e ^. eOperations . to (M.lookup v))
  case ml of
    Just l  -> return l
    Nothing -> throwError $ UnknownOperation v

lookupEff :: (MonadReader Environment m, MonadError Error m) => TyLit -> m [Ident]
lookupEff v = do
  ml <- asks (\e -> e ^. eEffects . to (M.lookup v))
  case ml of
    Just l  -> return l
    Nothing -> throwError $ UnknownEffect v

inEnv :: (MonadReader Environment m) => Ident -> Scheme -> m a -> m a
inEnv v s = local (eTypeContext %~ M.insert v s)

extendEnv :: Ident -> Scheme -> Environment -> Environment
extendEnv id s = eTypeContext %~ M.insert id s

combine :: Environment -> Environment -> Environment
combine (Env tcs1 ops1 eff1) (Env tcs2 ops2 eff2) =
  Env (M.union tcs1 tcs2) (M.union ops1 ops2) (M.union eff1 eff2)