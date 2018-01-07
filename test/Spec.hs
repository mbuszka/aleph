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

import Error
import Grammar
import Check

test :: Either Error Typ
test = do
  t <- parse "fn x -> fn y -> (fn a -> print 4) (y x)"
  fst $ runCheck $ process t

main :: IO ()
main = case test of
    Left err -> print err
    Right t -> putStrLn $ pShow t
