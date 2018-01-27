module Evaluation where

import Control.Monad.Except
import Control.Monad.Writer

import Evaluation.Types
import Evaluation.Eval as Eval
import Syntax.Grammar

import Builtins
import Error


eval :: (MonadIO m) => [Top] -> m (Maybe Error, [ExpVal])
eval p = do
  (err, vs) <- runWriterT (runExceptT (Eval.evalP evalEnv evalCtx p))
  return 
    ( case err of
        Left e  -> Just e
        Right _ -> Nothing
    , vs)