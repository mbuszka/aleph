{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
#-}

import Control.Lens

import Control.Monad
import Control.Monad.Except

import Data.Text    as Text
import Data.Text.IO as Text

import System.Directory

import Inference
import Error
import Syntax
import Print
import Evaluation


testFile :: (MonadIO m) => FilePath -> m ()
testFile f = do
  t <- liftIO $ Text.readFile f
  err <- runExceptT $ do
    p <- parse t
    e <- check p
    return (p, e)
  liftIO $ putDocW 80 $ "Testing file" <+> pretty f <> line <> line
  case err of
    Left e -> reportError e
    Right (p, e) -> do
      liftIO $ putDocW 80 $ pretty e
      (e, res) <- eval p
      case e of
        Just e  -> reportError e
        Nothing -> liftIO $ putDocW 80 $ "Program finished successfully" <> line
      liftIO $ putDocW 80 $ "Results:" <+> indent 2 (align $ vsep (fmap pretty res)) <> line <> line

reportError :: (MonadIO m) => Error  -> m ()
reportError e = liftIO $ putDocW 80 $ pretty e <> line

main :: IO ()
main = do
  dir <- getCurrentDirectory
  let files = [ dir ++ "/test/test-" ++ show i ++ ".al" | i <- [0 .. 3]]
  mapM_ testFile files
