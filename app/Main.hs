module Main where

import Control.Monad
import Control.Monad.Except

import Data.Text    as Text
import Data.Text.IO as Text

import System.Environment
import System.Exit

import Inference
import Inference.Env
import Error
import Syntax
import Print
import Evaluation

data Command
  = Check [FilePath]
  | Eval  [FilePath]

parseFiles :: (MonadIO m, MonadError Error m) => [FilePath] -> m [Top]
parseFiles fs = do
  ts <- liftIO $ mapM Text.readFile fs
  Prelude.concat <$> mapM parse ts

parseArgs :: (MonadIO m) => [String] -> m Command
parseArgs ("check" : fs) = return $ Check fs
parseArgs ("eval"  : fs) = return $ Eval fs
parseArgs _              = do
  liftIO $ putDocW 80 "usage: aleph-exe <eval | check> [files]"
  liftIO $ exitFailure

main :: IO ()
main = do
  args <- getArgs
  cmd  <- parseArgs args
  case cmd of
    Check fs -> do
      errs <- runExceptT $ parseFiles fs
      ps <- reportAndExit errs
      t <- typecheck ps
      liftIO $ putDocW 80 $ pretty t
    Eval fs  -> do
      errs <- runExceptT $ parseFiles fs
      ps <- reportAndExit errs
      typecheck ps
      (e, res) <- eval ps
      case e of
        Just e  -> liftIO $ putDocW 80 $ pretty e <> line
        Nothing -> liftIO $ putDocW 80 $ "Program finished successfully" <> line
      liftIO $ putDocW 80 $ "Results:" 
        <+> indent 2 (align $ vsep (fmap pretty res)) <> line <> line

reportAndExit :: (MonadIO m) => Either Error a -> m a
reportAndExit (Left e) = do
  liftIO $ putDocW 80 $ pretty e <> line
  liftIO $ exitFailure
reportAndExit (Right v) = return v

typecheck :: [Top] -> (MonadIO m) => m Env
typecheck p = do
  e <- runExceptT $ check p
  reportAndExit e
