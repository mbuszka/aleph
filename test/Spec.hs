import           System.IO
import qualified System.IO.Strict as S
import qualified Data.Map as M

import Control.Monad

import Data.Bifunctor

import Parser
import Check
import Type

main :: IO ()
main = case parse term "(fn x -> fn y -> x) true" of
    Left err -> print err
    Right t -> do
      let (ty, cs) = runCheck (process t)
      print $ printType <$> ty -- map (bimap printType printType) cs