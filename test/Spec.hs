{-# LANGUAGE
    TemplateHaskell
  , QuasiQuotes
  , FlexibleContexts
  , FlexibleInstances
#-}

import           System.IO
import qualified System.IO.Strict as S
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Except

import Data.Bifunctor

import Check
import Error
import Grammar
import Parse

unEither :: (MonadError Error m, MonadIO m) => Either Error a -> m a
unEither x = case x of
  Left e -> throw e
  Right t -> return t

test :: (MonadError Error m, MonadIO m) => String -> m ()
test s = do
  t <- unEither $ parse s
  let (p, cs) = runCheck $ process t
  (t, e) <- unEither p
  liftIO $ print cs
  liftIO $ putStrLn $ "  " ++ pShow t
  liftIO $ putStrLn $ "  " ++ pShow e

main :: IO ()
main = do
  putStrLn ""
  mapM_ (\s -> putStrLn s >> (runExceptT (test s)) >> putStrLn "")
    [ "5"
    , "fn x -> 5"
    , "(fn x -> 5) ()"
    , "print"
    , "print 5"
    , "fn x -> print x"
    , "fn x -> fn y -> (fn a -> print 4) (y x)"
    , "fn x -> fn y -> z <- y x; print 5"
    , "let id = fn x -> x in id id"
    , "fn x -> x (x 5)"
    , "fn x -> x 5 (x ())"
    ]
