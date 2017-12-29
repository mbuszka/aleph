module Check where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

import qualified Data.Map as M
import           Data.Map(Map)
import qualified Data.Set as S
import           Data.Set(Set)
import Data.List(sort)

import Data.Generics

import Syntax
import NameSupply
import Renamer(withRename)

data Error = Err String
data Context = Ctx
  { boundRowVars     :: Set String
  , boundVars        :: Map String Type
  , operationToLabel :: Map String String
  }

type CheckMonad a = ExceptT Error (RWST () () NameStore IO) a

mrgRow (ROpen l1 v) lbls = ROpen (sort $ l1 ++ lbls) v
mrgRow (RClosed l1) lbls = RClosed $ sort $ l1 ++ lbls

substInRow :: String -> Row -> Row -> Row
substInRow str tgt row@(ROpen lbls id) = if id == str then mrgRow tgt lbls else row
substInRow str tgt row = row

substInType :: (Typeable a) => String -> Row -> a -> a
substInType str tgt = mkT $ substInRow str tgt

tyEq :: Type -> Type -> ReaderT (Map String String) (State NameStore) Bool
tyEq (TyLit l1) (TyLit l2)
  | l1 == l2 = return True
tyEq (TyArrow t1 r1 t1') (TyArrow t2 r2 t2') = do
  b1 <- tyEq t1 t2
  b2 <- rowEq r1 r2
  b3 <- tyEq t1' t2'
  return $ b1 && b2 && b3
tyEq (TyForall bind1 t1) (TyForall bind2 t2) =
  withRename bind1 bind2 $ tyEq t1 t2
tyEq _ _ = return False

rowEq :: Row -> Row -> ReaderT (Map String String) (State NameStore) Bool
rowEq (RClosed l1) (RClosed l2) = return $ l1 == l2
rowEq (ROpen l1 v1) (ROpen l2 v2) = return $ l1 == l2 && v1 == v2
rowEeq _ _ = return False

-- typeOf :: Context -> Expression -> CheckMonad (Type, Row)
-- typeOf ctx expr = case expr of 
--   TyApp ex targetType -> do
--     tyEx <- typeOf ctx ex
--     case fst tyEx of
--       TyForall var ty -> return $ substRowVar var targetType ty
--       _               -> throwError $ Err $ 
--                           "Expected forall quantifier, got: " ++ show tyEx
--   App l r -> do
--     tl <- typeOf ctx l
--     tr <- typeOf ctx r
--     case tl of
--       TyArrow tyf row tyt -> if tyf \= tr
--         then throwError $ Err $
--           "Function and operator type mismatch, expected: " ++ show tyf
--             ++ "got: " ++ show tr
--         else return $ (tyt, row)
--       otherTy -> throwError $ Err $
--                    "Expected function type, got: " ++ show otherTy