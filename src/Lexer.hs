module Lexer
  ( Parser
  , lexeme
  , number
  , string
  , typeVar
  , typeLit
  , var
  , parens
  , squares
  , reserved
  , reservedOp
  ) where

import qualified Text.Parsec          as P
import qualified Text.Parsec.Token    as P
import qualified Text.Parsec.Language as P
import           Data.Set(Set, member, fromList)

import Syntax
import Type

type Parser a = P.Parsec String () a

spec :: P.LanguageDef ()
spec = P.haskellStyle 
  { P.reservedOpNames = 
    [ ","
    , "."
    , ":"
    , "="
    , "."
    , "->"
    ]
  }

resNames :: Set Identifier
resNames = fromList
  [ "let"
  , "rec"
  , "fn"
  , "fa"
  , "in"
  , "run"
  , "handle"
  , "with"
  , "()"
  ]

resOps :: Set Identifier
resOps = fromList $ P.reservedOpNames spec
  
lexer :: P.TokenParser ()
lexer = P.makeTokenParser spec

letter :: Parser Char
letter = P.identLetter spec

number :: Parser Integer
number = P.integer lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

typeLit :: Parser Identifier
typeLit = lexeme $ (:) <$> P.upper <*> P.many letter

typeVar :: Parser TVar
typeVar = TV <$> lexeme ((:) <$> P.char '\'' <*> P.many letter)

var :: Parser Var
var = V <$> lexeme (do
  id <- (:) <$> P.lower <*> P.many letter
  if id `member` resNames
    then fail (id ++ " is a reserved name")
    else return id)

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

squares = P.squares lexer

string :: Parser String
string = P.stringLiteral lexer