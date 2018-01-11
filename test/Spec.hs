{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
#-}

import           System.IO
import qualified System.IO.Strict as S

import qualified Data.Map  as M
import qualified Data.Text as T
import           Data.Text(Text)
import           Data.Text.Prettyprint.Doc

import Control.Lens

import Control.Monad
import Control.Monad.Except

import Data.Bifunctor

import Check
import Error
import Grammar
import Parse
import Print

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
