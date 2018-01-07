{-# LANGUAGE
    TemplateHaskell
  , QuasiQuotes
  , FlexibleContexts
  , FlexibleInstances
#-}

module Grammar 
  ( Top(..)
  , Term(..) 
  , Handler(..)
  , Val(..)
  , Typ(..)
  , Row(..)
  , Parse(..)
  , pShow
  ) where

import Control.Monad.Except
import Data.Char
import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc)
import Language.LBNF.Runtime(printTree, doc, Doc)

import Error

class Parse a where
  parse :: (MonadError Error m) => String -> m a

bnfc [lbnf|

Def.       Top ::= "let" Ident "=" Term;
Run.       Top ::= "run" Term;

terminator Top "";

token TVar ('\'' (letter | digit)* );

App.       Term1 ::= Term1 Term2;
Let.       Term  ::= "let" Ident "=" Term "in" Term;
Abs.       Term  ::= "fn" Ident "->" Term ;
Var.       Term2 ::= Ident;
Lit.       Term2 ::= Val;
Handle.    Term  ::= "handle" Term "with" [Handler];
Lift.      Term  ::= "lift" Term2;
coercions  Term  2;

Op.        Handler ::= Ident Ident "," Ident "->" Term;
Ret.       Handler ::= "return" Ident "->" Term;
terminator Handler ";" ;

VInt.      Val ::= Integer;
VUnit.     Val ::= "()";

TyLit.     Typ1 ::= Ident;
TyArr.     Typ  ::= Typ1 "->" Typ2 Typ;
TyVar.     Typ2 ::= TVar;
TyRow.     Typ2 ::= "<" [Typ1] Row;
coercions  Typ 2;

separator  Typ1 ",";

Open.      Row  ::= "|" TVar ">";
Closed.    Row  ::= ">";
-- coercions  Row 1;

|]

instance Parse Top where
  parse s = case pTop $ myLexer s of
    Ok t  -> return t
    Bad s -> throw $ ParseError s 

instance Parse Typ where
  parse s = case pTyp $ myLexer s of
    Ok t  -> return t
    Bad s -> throw $ ParseError s

instance Parse [Top] where
  parse s = case pListTop $ myLexer s of
    Ok t  -> return t
    Bad s -> throw $ ParseError s

instance Parse Term where
  parse s = case pTerm $ myLexer s of
    Ok t  -> return t
    Bad s -> throw $ ParseError s

pShow :: Print a => a -> String
pShow = printTree