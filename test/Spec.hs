import System.IO
import qualified System.IO.Strict as S
import Control.Monad

import Parser
import Renamer
import Common
import Syntax

checkOne num = 
  withFile ("/home/mbuszka/university/aleph/test/test-" ++ show num ++ ".al") ReadMode $
    \handle -> do
      hSetEncoding handle utf8
      t <- S.hGetContents handle
      return $ do
        ts <- parse program t
        renameAll ts

main :: IO ()
main = do
  mapM_ (checkOne >=> print) [1 .. 2]
  return ()

