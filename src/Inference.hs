module Inference where

import Control.Monad.Except
import Control.Monad.RWS

import Inference.Env
import Inference.Infer
import Inference.Types
import Syntax.Grammar

import Builtins
import Error

check :: (MonadIO m, MonadError Error m) => [Top] -> m Env
check p = fst <$> evalRWST (processProgram p) typeEnv initState