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

import Inference.Infer
import Error
import Syntax.Grammar
import Syntax.Parse
import Print
import Evaluation.Eval
import Inference.Environment(_eOperations)


testFile :: (MonadError Error m, MonadIO m) => FilePath -> m ()
testFile f = do
  t <- liftIO $ TIO.readFile f
  p <- parse program t
  e <- evalCheck $ processProgram p
  -- liftIO $ putDocW 80 $ pretty e <> line
  ((), res) <- runEval $ evalProgram p
  liftIO $ putDocW 80 $ pretty res <> line
  return ()

reportError :: (MonadIO m) => ExceptT Error m a -> m ()
reportError x = do
  e <- runExceptT x
  case e of
    Left err -> liftIO $ putDocW 80 $ pretty err
    Right x  -> return ()

main :: IO ()
main = mapM_ 
  (\i ->
    let s = "/home/mbuszka/university/aleph/test/test-" ++ show i ++ ".al"
    in reportError (testFile s) >> putStrLn "")
  [ 0 .. 2 ]
