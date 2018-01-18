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

unEither :: (MonadError Error m, MonadIO m) => Either Error a -> m a
unEither x = case x of
  Left e -> throw e
  Right t -> return t

test :: (MonadError Error m, MonadIO m) => Text -> m ()
test s = do
  t <- parse term s
  liftIO $ putDocW 80 $ pretty t <> line
  (p, st, cs) <- liftIO $ runCheck $ process t
  (t, e) <- unEither p
  liftIO $ putDocW 80 $ -- pretty cs <> line <> 
                        pretty t <+> "|" <+> pretty e <> line


testFile :: (MonadError Error m, MonadIO m) => FilePath -> m ()
testFile f = do
  t <- liftIO $ TIO.readFile f
  p <- parse program t
  e <- run $ processProgram p
  liftIO $ putDocW 80 $ pretty e
  let ops = fmap (\(l, _, _) -> l) $ _eOperations e
  ((), res) <- runEval (initialCtx ops) (evalProgram p)
  liftIO $ putDocW 80 $ pretty res
  return ()

run c = unEither =<< liftIO (evalCheck c)

testTop :: (MonadError Error m, MonadIO m) => Text -> m ()
testTop s = do
  t <- parse top s
  case t of
    Def id t -> do
      scheme <- run $ processDef t
      liftIO $ putDocW 80 $ pretty id <+> ":" <+> pretty scheme <> line
    Run t -> do
      (t, e) <-run $ process t
      liftIO $ putDocW 80 $ pretty t <+> "|" <+> pretty e <> line
    e@EffDef{} ->
      liftIO $ putDocW 80 $ pretty e <> line

reportError :: (MonadIO m) => ExceptT Error m a -> m ()
reportError x = do
  e <- runExceptT x
  case e of
    Left err -> liftIO $ putDocW 80 $ pretty err
    Right x  -> return ()

main :: IO ()
main = do
  putStrLn ""
  mapM_ (\s -> reportError (test s) >> putStrLn "")
    [ "5"
    , "fn x -> 5"
    , "(fn x -> 5) ()"
    , "print"
    , "print 5"
    , "fn x -> print x"
    , "handle IO in print 5 with \
      \ print x, r -> r (); \
      \ return x -> (); "
    , "(handle ST in u <- put 5, get () with \
       \ put x, r -> fn s -> (r ()) x; \
       \ get u, r -> fn s -> (r s) s;  \
       \ return x -> fn s -> s;)"
    ]
  mapM_ (\s -> reportError (testTop s) >> putStrLn "")
    [ "let f = fn x -> print x"
    , "run print 5"
    , "eff Reader = \
      \ ask : Unit -> Int;"
    ]
  mapM_ (\s -> reportError (testFile s) >> putStrLn "")
    [ "/home/mbuszka/university/aleph/test/test-1.al"
    ]
