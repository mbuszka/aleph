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
  (p, st, cs) <- liftIO $ runCheck $ process t
  (t, e) <- unEither p
  liftIO $ putDocW 80 $ pretty cs <> line <> pretty t <+> "|" <+> pretty e <> line

main :: IO ()
main = do
  putStrLn ""
  mapM_ (\s -> putStrLn (T.unpack s) >> (runExceptT (test s)) >> putStrLn "")
    [ "5"
    -- , "fn x -> 5"
    -- , "(fn x -> 5) ()"
    -- , "print"
    , "print 5"
    -- , "fn x -> print x"
    -- , "fn x -> fn y -> (fn a -> print 4) (y x)"
    -- , "fn x -> fn y -> z <- y x; print 5"
    -- , "let id = fn x -> x in id id"
    -- , "fn x -> x (x 5)"
    -- , "fn x -> x 5 (x ())"
    ]
