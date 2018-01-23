{-# LANGUAGE
    OverloadedStrings
#-}

module Syntax.Lex 
  ( ident
  , parens
  , res
  , resOp
  , str
  , tyLit
  , tyVar
  , val
  , whitespace
  ) where

import           Control.Applicative
import           Control.Monad.Identity

import           Data.Text(Text)
import qualified Data.Text         as T
import           Data.Set(Set)
import qualified Data.Set          as S 

import           Text.Parsec.Language(haskellStyle)
import qualified Text.Parsec       as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Char  as P

import Syntax.Grammar

lexer :: P.GenTokenParser Text () Identity
lexer = P.makeTokenParser $ P.LanguageDef
  { P.commentStart    = "{-"
  , P.commentEnd      = "-}"
  , P.commentLine     = "--"
  , P.nestedComments  = True
  , P.identStart      = P.alphaNum
  , P.identLetter     = P.alphaNum
  , P.opStart         = P.oneOf "-<>"
  , P.opLetter        = P.oneOf "-<>"
  , P.reservedNames   = map T.unpack $ S.toList reservedTokens
  , P.reservedOpNames = [ "=", "->", "=>", ":", "," ]
  , P.caseSensitive   = True
  }

lexeme = P.lexeme lexer

identChar = P.alphaNum <|> P.oneOf "'_"

reservedTokens :: Set Text
reservedTokens = S.fromList
  [ "let"
  , "letrec"
  , "run"
  , "in"
  , "eff"
  , "fn"
  , "if"
  , "then"
  , "else"
  , "end"
  , "handle"
  , "with"
  , "return"
  , "true"
  , "false"
  ]

whitespace = P.whiteSpace lexer

isReserved txt = S.member txt reservedTokens

res = P.reserved lexer

resOp = P.reservedOp lexer

parens = P.parens lexer

str s = lexeme $ P.string s

val =  VInt <$> P.integer lexer
   <|> pure VUnit <* P.try (str "()")
   <|> (VBool True <$ res "true")
   <|> (VBool False <$ res "false")

tyVar = lexeme $ P.try $ do
  t <- (do
    c <- P.lower
    cs <- P.many identChar
    return $ T.pack $ c:cs) P.<?> "type variable"
  if isReserved t
    then P.unexpected $ T.unpack t ++ "is a reserved keyword"
    else return $ TV t

tyLit = lexeme $ P.try $ do
  t <- (do
    c <- P.upper
    cs <- P.many identChar
    return $ T.pack $ c:cs) P.<?> "type literal"
  if isReserved t
    then P.unexpected $ T.unpack t ++ "is a reserved keyword"
    else return $ TL t

ident = lexeme $ P.try $ do
  t <- (do
    c <- P.letter
    cs <- P.many identChar
    return $ T.pack $ c:cs) P.<?> "identifier"
  if isReserved t
    then P.unexpected $ T.unpack t ++ "is a reserved keyword"
    else return $ ID t