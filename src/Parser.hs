module Parser where

import qualified Text.Parsec          as P
import qualified Text.Parsec.Token    as P
import qualified Text.Parsec.Language as P

import Control.Applicative

import Syntax
import Type
import Lexer

parse :: Parser a -> String -> Either P.ParseError a
parse p = P.parse p ""

top :: Parser Top
top =  TLet <$> (reserved "let" >> binding) <*> (reservedOp "=" >> term)
   <|> TRun <$> (reserved "run" >> term)

binding :: Parser Binding
binding = Bind <$> var <*> return Nothing -- optional tyTerm

term :: Parser Term
term =  Let <$> (reserved "let" >> binding) <*> (reservedOp "=" >> term) <*> (reserved "in" >> term)
    <|> Handle <$> (reserved "handle" >> term) <*> (reserved "with" >> handlers)
    <|> Lift <$> (reserved "lift" >> typeLit) <*> aTerm
    <|> app
    <|> aTerm
    where
      handlers = many (handler <|> ret)
      handler = Handler <$> var 
                        <*> var 
                        <*> (reservedOp "," >> var)
                        <*> (reservedOp "->" >> term)
      ret = Ret <$> (reserved "return" >> var) <*> (reservedOp "->" >> term)

app :: Parser Term
app = do
  ts <- P.many1 aTerm
  return $ foldl1 App ts

aTerm :: Parser Term
aTerm =  parens term
     <|> Abs <$> (reserved "fn" >> binding) <*> (reservedOp "->" >> term)
     <|> Lit <$> litVal
     <|> Var <$> var

litVal :: Parser Val
litVal =  VInt <$> number
      <|> VBool <$> bool
      <|> (reserved "()" >> return VUnit)
      where
        bool =  (reserved "true" >> return True)
            <|> (reserved "false" >> return False)

-- tyTerm :: Parser Type
-- tyTerm =  (P.try $ TyArr <$> atyTerm <*> (reservedOp "->" >> row) <*> tyTerm)
--       <|> atyTerm

-- atyTerm :: Parser Type
-- atyTerm =  TyLit <$> typeLit
--        <|> parens tyTerm

-- row :: Parser Row
-- row =  ROpen <$> typeVar
--    <|> squares fullRow
--     where
--       fullRow = do
--         ls <- many typeLit
--         tlm <- optional (reservedOp "|" >> typeVar)
--         let 
--           tl = case tlm of 
--             Just v -> ROpen v
--             Nothing -> RNil
--         return $ foldr RCons tl ls
