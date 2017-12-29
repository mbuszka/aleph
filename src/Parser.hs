module Parser where

import qualified Text.Parsec as Par
import qualified Data.Set    as S

import Control.Applicative
import Data.Functor.Identity
import Data.Bifunctor
import Data.List(sort)

import Syntax
import Lexer
import Common

type ParseState = ()
type ParseMonad = Identity
type Parser a = Par.ParsecT String ParseState ParseMonad a

parse :: Parser a -> String -> Either Error a
parse p = first ParseError . Par.runParser p () ""

program :: Parser [TopLevel]
program = whitespace >> many topLevel

boolean :: Parser Bool
boolean = (Par.string "true" >> return True) <|> (Par.string "false" >> return False)

literal :: Parser Literal
literal =  (B <$> boolean) 
       <|> (I . fromIntegral <$> integer)
       <|> (S <$> stringLit)
       <|> (reservedOp "()" >> (return U))

-- tyLambda :: Parser Value
-- tyLambda = do
--   reserved "Fun" <|> reserved "Λ"
--   id <- identifier
--   reservedOp "."
--   ex <- expression
--   return $ TyLambda id ex

varBinding :: Parser VarBinding
varBinding = do
  id <- varIdent
  ty <- optional $ do
    reservedOp ":"
    typeTerm
  return $ VBind id ty

typeBinding :: Parser TypeBinding
typeBinding = do
  id <- typeIdent
  return $ TBind id KRow

lambda :: Parser Value
lambda = do
  reserved "fun" <|> reserved "λ"
  b <- (BVar <$> Par.try varBinding <|> BTyp <$> typeBinding) Par.<?> "Expected variable or type binding"
  reservedOp "."
  ex <- expression
  return $ Lambda b ex 

value :: Parser Value
value = lambda <|> (Lit <$> literal) <|> (Variable <$> identifier)

letexp :: Parser Expression
letexp = do
  reserved "let"
  binder <- varBinding
  reservedOp "="
  bound <- expression
  reserved "in"
  body <- expression
  return $ LetExp binder bound body

term :: Parser Expression
term =  (Val <$> value)
    <|> (reserved "lift" >> expression)
    <|> (inParens expression)
    <|> RowExp <$> rowType

application :: Parser Expression
application = do
  e1 <- term
  e2 <- term
  return $ App e1 e2

expression :: Parser Expression
expression = letexp <|> term <|> application

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

forallType :: Parser Type
forallType = do
  reserved "forall"
  b <- typeBinding
  reserved "."
  t <- typeTerm
  return $ TyForall b t

typeTerm :: Parser Type
typeTerm = forallType <|> (Par.try arrowType) <|> atomType

rowType :: Parser Row
rowType = inAngles rowTerm <|> (ROpen S.empty <$> typeIdent)

rowTerm :: Parser Row
rowTerm = do
  lbls <- Par.sepBy identifier (reservedOp ",")
  var <- optional $ do
    reservedOp "|"
    typeIdent
  case var of
    Just v -> return $ ROpen (sort lbls) v
    Nothing -> return $ RClosed (sort lbls)

definition :: Parser TopLevel
definition = do
  reserved "def"
  b <- varBinding
  reservedOp "="
  val <- value
  return $ Definition b val

runnable :: Parser TopLevel
runnable = do
  reserved "run"
  Runnable <$> expression

topLevel :: Parser TopLevel
topLevel = definition <|> runnable