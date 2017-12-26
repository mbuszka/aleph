import Parser

import Control.Monad

parseOne num = do
  t <- readFile $ "/home/mbuszka/university/aleph/test/test-" ++ show num ++ ".al"
  return $ parse topLevel t

main :: IO ()
main = do
  mapM_ (parseOne >=> (putStrLn . show)) [1 .. 2]

