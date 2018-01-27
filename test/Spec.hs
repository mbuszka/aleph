{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
#-}

import           System.IO
import qualified System.IO.Strict as S

import qualified Data.Map     as M
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Text(Text)
import           Data.Text.Prettyprint.Doc

import Control.Lens

import Control.Monad
import Control.Monad.Except

import Data.Bifunctor

import Inference
import Error
import Syntax
import Print
import Evaluation


testFile :: (MonadIO m) => FilePath -> m ()
testFile f = do
  t <- liftIO $ TIO.readFile f
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
      liftIO $ putDocW 80 $ "Results:" <+> indent 2 (align $ vsep (map pretty res)) <> line <> line

reportError :: (MonadIO m) => Error  -> m ()
reportError e = liftIO $ putDocW 80 $ pretty e <> line

main :: IO ()
main = mapM_ 
  (\i ->
    let s = "/home/mbuszka/university/aleph/test/test-" ++ show i ++ ".al"
    in testFile s)
  [ 0 .. 3 ]
