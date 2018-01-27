{-# LANGUAGE
  FlexibleContexts
#-}

module Syntax.Parse where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity

import Data.Text(Text)

import qualified Text.Parsec as P
import           Text.Parsec(ParsecT)

import Error
import Syntax.Grammar
import Syntax.Lex

type Parser a = ParsecT Text () Identity a

parse :: (MonadError Error m) => Parser a -> Text -> m a
parse p s = case P.parse p "" s of
  Left err -> throw $ ParseError (show err)
  Right x  -> return x

program = whitespace *> (P.many top)

top =  Def <$> (res "let" >> ident) <*> (resOp "=" >> term)
   <|> Run <$> (res "run" >> term)
   <|> EffDef <$> (res "eff" >> tyLit) <*> (resOp "=" >> opDefs)

opDefs = P.endBy 
  (OpDef <$> ident <*> (resOp ":" >> typ) <*> (resOp "=>" >> typ))
  (resOp ";")

term =  Let    <$> (res "let" *> ident) <*> (resOp "=" *>  term) <*> (res "in" *> term)
    <|> LetRec <$> (res "letrec" *> ident) <*> ident <*> (resOp "->" *> term) <*> (res "in" *> term)
    <|> Abs    <$> (res "fn"  *> ident) <*> (resOp "->" *> term)
    <|> Handle <$> (res "handle" *> tyLit) <*> (res "in" *> term) <*> (res "with" *> handlers)
    <|> Lift   <$> (res "lift"   *> tyLit) <*> (res "in" *> parens term)
    <|> Cond   <$> (res "if" *> term) <*> (res "then" *> term) <*> (res "else" *> term <* res "end")
    <|> Bind   <$> (P.try (ident <* resOp "<-")) <*> term <*> (resOp "," *> term)
    <|> term1

term1 =  foldl1 App <$> P.many1 term2

term2 =  Var <$> ident
     <|> Lit <$> val
     <|> parens term

handlers = P.endBy (  Op  <$> ident <*> ident <*> (resOp "," *> ident) <*> (resOp "->" *> term)
                  <|> Ret <$> (res "return" *> ident) <*> (resOp "->" *> term)
                   ) (resOp ";")
              
typ =  (P.try $ TyArr <$> typ1 <*> (resOp "->" *> row) <*> typ)
   <|> typ1

typ1 = TyLit <$> tyLit
   <|> parens typ

row =  Row [] . Just <$> tyVar
   <|> Row <$> (str "[" *> P.many tyLit) <*> (optional (str "|" *> tyVar) <* str "]")