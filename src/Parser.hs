module Parser where

import qualified Text.Parsec      as Par

import Control.Applicative
import Data.Functor.Identity

import Syntax
import Lexer

type ParseState = ()
type ParseMonad = Identity
type Parser a = Par.ParsecT String ParseState ParseMonad a

parse :: Parser a -> String -> Either Par.ParseError a
parse p = Par.runParser p () ""

boolean :: Parser Bool
boolean = (Par.string "true" >> return True) <|> (Par.string "false" >> return False)

literal :: Parser Literal
literal =  (B <$> boolean) 
       <|> (I . fromIntegral <$> integer)
       <|> (S <$> stringLit)
       <|> (reservedOp "()" >> (return U))

lambda :: Parser Value
lambda = do
  reserved "fun"
  id <- identifier
  reservedOp "=>"
  ex <- expression
  return $ Lambda id ex 

value :: Parser Value
value = lambda <|> (Lit <$> literal) <|> (Variable <$> identifier)

term :: Parser Expression
term = (Val <$> value) <|> (reserved "lift" >> expression) <|> (inParens expression)

application :: Parser Expression
application = do
  e1 <- term
  e2 <- term
  return $ Application e1 e2

expression :: Parser Expression
expression = term <|> application

atomType :: Parser Type
atomType =  (reserved "()" >> return TyUnit)
        <|> (reserved "Int" >> return TyInt)
        <|> (reserved "Bool" >> return TyBool)
        <|> (inParens typeTerm)

arrowType :: Parser Type
arrowType = do
  l <- atomType
  reservedOp "->"
  row <- rowType
  r <- typeTerm
  return $ TyArrow l row r

typeTerm :: Parser Type
typeTerm = (Par.try arrowType) <|> atomType

rowType :: Parser Row
rowType = inAngles rowTerm <|> (RVar <$> typeVar)

rowTerm :: Parser Row
rowTerm = do
  idents <- Par.many typeIdent
  rest <- optional $ do
    reservedOp "|"
    typeVar
  return $ case rest of
    Just v  -> Open idents v
    Nothing -> Closed idents


definition :: Parser TopLevel
definition = do
  reserved "def"
  id <- identifier
  typ <- Par.optionMaybe (do
    reservedOp ":"
    typeTerm)
  reservedOp "="
  val <- value
  return $ Definition id typ val

runnable :: Parser TopLevel
runnable = do
  reserved "run"
  Runnable <$> expression


topLevel :: Parser TopLevel
topLevel = definition <|> runnable