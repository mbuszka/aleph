module Lexer where

import qualified Text.Parsec       as Par
import qualified Text.Parsec.Token as Par

import Control.Applicative

import Syntax

type LexerDef st m = Par.GenTokenParser String st m
type Spec  st m = Par.GenLanguageDef String st m
type Lexer st m a = Par.ParsecT String st m a

lexer :: (Monad m) => LexerDef st m
lexer = Par.makeTokenParser spec

spec :: (Monad m) => Spec st m
spec = Par.LanguageDef
  { Par.commentStart    = "{-"
  , Par.commentEnd      = "-}"
  , Par.commentLine     = "--"
  , Par.nestedComments  = True
  , Par.identStart      = Par.letter <|> Par.char '_'
  , Par.identLetter     = Par.alphaNum <|> Par.oneOf "_"
  , Par.opStart         = Par.oneOf ":=(-<>|"
  , Par.opLetter        = Par.oneOf ":=>()-<>|"
  , Par.reservedNames   = reservedNames
  , Par.reservedOpNames = reservedOpNames
  , Par.caseSensitive   = True
  }

reservedNames :: [String]
reservedNames =
  [ "def"
  , "fun"
  , "let"
  , "true"
  , "false"
  , "lift"
  , "handle"
  , "with"
  , "Int"
  , "Bool"
  ]

reservedOpNames :: [String]
reservedOpNames =
  [ ":"
  , "=>"
  , "()"
  , "->"
  , "<"
  , ">"
  , "|"
  ]

reservedOp :: (Monad m) => Identifier -> Lexer st m ()
reservedOp = Par.reservedOp lexer

integer :: (Monad m) => Lexer st m Integer
integer = Par.integer lexer

identifier :: (Monad m) => Lexer st m Identifier
identifier = Par.identifier lexer

lexeme :: (Monad m) => Lexer st m a -> Lexer st m a
lexeme = Par.lexeme lexer

typeIdent :: (Monad m) => Lexer st m Identifier
typeIdent = lexeme $ do
  c <- Par.oneOf ['A' .. 'Z']
  rest <- Par.many (Par.identLetter spec)
  return $ c:rest

typeVar :: (Monad m) => Lexer st m Identifier
typeVar = lexeme $ do
  c <- Par.oneOf ['a' .. 'z' ]
  rest <- Par.many (Par.identLetter spec)
  return $ c:rest

stringLit :: (Monad m) => Lexer st m String
stringLit = Par.stringLiteral lexer

reserved :: (Monad m) => Identifier -> Lexer st m ()
reserved = Par.reserved lexer

inParens :: (Monad m) => Lexer st m a -> Lexer st m a
inParens = Par.parens lexer

inAngles :: (Monad m) => Lexer st m a -> Lexer st m a
inAngles = Par.angles lexer
