{-# LANGUAGE 
    ConstraintKinds
  , FlexibleContexts
  , OverloadedStrings
#-}

module Inference.Solve where

import Control.Monad.Writer

import qualified Data.List as List

import Inference.Types
import Inference.Subst as Subst

import Error
import Print
import Syntax

unifyT :: Solve m => Subst -> (Typ, Typ) -> m (Subst, Constraints)
unifyT s (a, b) | a == b = return (s, [])
unifyT s (TyVar a, b) = (,) <$> Subst.extend a (Left b) s <*> return []
unifyT s (a, TyVar b) = (,) <$> Subst.extend b (Left a) s <*> return []
-- unifyT s (TyLit a, TyLit b) | a == b = return (s, [])
unifyT s (TyArr a1 r1 b1,  TyArr a2 r2 b2) = 
  let cs = [ TyConstr a1 a2
            , RoConstr r1 r2
            , TyConstr b1 b2
            ]
  in do
    tell cs
    return (s, cs)
unifyT s (t1, t2) = throw $ UnificationError $ pretty t1 <+> "and" <+> pretty t2


unifyR :: Solve m => Subst -> (Row, Row) -> m (Subst, Constraints)
unifyR s (r1@(Row l1 v1), r2@(Row l2 v2)) = let
  extraInL1 = l1 List.\\ l2
  extraInL2 = l2 List.\\ l1 in
    case (v1, v2) of
      (Nothing, Nothing) -> 
        if l1 /= l2 
        then throw $ UnificationError $ pretty r1 <+> "and" <+> pretty r2
        else return (s, [])
      
      (Just a, Nothing) ->
        if extraInL1 /= []
        then throw $ UnificationError $ pretty r1 <+> "and" <+> pretty r2
        else do
          s' <- Subst.extend a (Right $ Row extraInL2 Nothing) s
          return $ (s', [])
      
      (Nothing, Just a) ->
        unifyR s (Row l2 v2, Row l1 v1)
      
      (Just a, Just b) -> do
        fr <- fresh
        when (a == b && (List.sort l1) /= (List.sort l2)) $ 
          throw $ UnificationError $ 
            "recursive types:" <+> pretty r1 <+> "and" <+> pretty r2
        s' <- Subst.extend a (Right $ Row extraInL2 (Just fr)) s >>=
                Subst.extend b (Right $ Row extraInL1 (Just fr))
        -- liftIO $ putDocW 80 $ "resulting subst:" P.<+> pretty s' P.<> P.line
        return (s', [])
    
solve :: Solve m => Subst -> Constraints -> m Subst
solve s (c:cs) = do
  c' <- apply s c
  (s', cs') <- case c' of
    TyConstr a b -> unifyT s (a, b)
    RoConstr a b -> unifyR s (a, b)
  -- cs'' <- apply s' (cs' ++ cs)
  solve s' (cs ++ cs')
solve s [] = return s