{-# LANGUAGE
    TemplateHaskell
  , QuasiQuotes
  , FlexibleContexts
  , FlexibleInstances
#-}

module Parse where

import Control.Monad.Except
import Language.LBNF.Compiletime

import Error
import Grammar
import Grammar.Impl

class Parse a where
  parse :: (MonadError Error m) => String -> m a

instance Parse Top where
  parse s = case pTop $ myLexer s of
    Ok t  -> return t
    Bad s -> throw $ ParseError s 

instance Parse Typ where
  parse s = case pSTyp $ myLexer s of
    Ok t  -> return $ fromSTyp t
    Bad s -> throw $ ParseError s

instance Parse [Top] where
  parse s = case pListTop $ myLexer s of
    Ok t  -> return t
    Bad s -> throw $ ParseError s

instance Parse Term where
  parse s = case pTerm $ myLexer s of
    Ok t  -> return t
    Bad s -> throw $ ParseError s
