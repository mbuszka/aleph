{-# LANGUAGE
  FlexibleContexts
#-}

module Parse where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity

import Data.Text(Text)

import qualified Text.Parsec as P
import           Text.Parsec(ParsecT)

import Error
import Grammar
import Lex

type Parser a = ParsecT Text () Identity a

parse :: (MonadError Error m) => Parser a -> Text -> m a
parse p s = case P.parse p "" s of
  Left err -> throw $ ParseError (show err)
  Right x  -> return x

top =  Def <$> (res "let" >> ident) <*> (resOp "=" >> term)
   <|> Run <$> (res "run" >> term)
   <|> EffDef <$> (res "eff" >> tyLit) <*> (resOp "=" >> opDefs)

opDefs = P.endBy (OpDef <$> ident <*> (resOp ":" >> typ) <*> (resOp "->" >> typ)) (resOp ";")

term =  Let    <$> (res "let" *> ident) <*> (resOp "=" *>  term) <*> (res "in" *> term)
    <|> Abs    <$> (res "fn"  *> ident) <*> (resOp "->" *> term)
    <|> Handle <$> (res "handle" *> tyLit) <*> (res "in" *> term) <*> (res "with" *> handlers)
    <|> Lift   <$> (res "lift"   *> tyLit) <*> (res "in" *> term2)
    <|> (P.try $ Bind   <$> (ident <* resOp "<-") <*> term <*> (resOp ";" *> term))
    <|> term1

term1 =  foldl1 App <$> P.many1 term2

term2 =  Var <$> ident
     <|> Lit <$> val
     <|> parens term

handlers = many  (  Op  <$> ident <*> ident <*> (resOp "," *> ident) <*> (resOp "->" *> term)
                <|> Ret <$> (res "return" *> ident) <*> (resOp "->" *> term)
                 )
              
typ =  (P.try $ TyArr <$> typ1 <*> (resOp "->" *> row) <*> typ)
   <|> typ1

typ1 = TyLit <$> tyLit

row =  Row [] . Just <$> tyVar
   <|> Row <$> (str "[" *> P.many tyLit) <*> (optional (str "|" *> tyVar) <* str "]")