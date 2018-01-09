{-# LANGUAGE
    TemplateHaskell
  , QuasiQuotes
  , FlexibleContexts
  , FlexibleInstances
#-}

module Grammar.Impl where

import Language.LBNF.Compiletime
import Language.LBNF(lbnf, bnfc)

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
Lift.      Term  ::= "lift" Ident Term2;
Bind.      Term  ::= Ident "<-" Term ";" Term;
-- Seq.       Term  ::= Term ";" Term;
coercions  Term  2;

Op.        Handler ::= Ident Ident "," Ident "->" Term;
Ret.       Handler ::= "return" Ident "->" Term;
terminator Handler ";" ;

VInt.      Val ::= Integer;
VUnit.     Val ::= "()";

SLit.      STyp1 ::= Ident;
SArr.      STyp  ::= STyp1 "->" SRow STyp;
SVar.      STyp2 ::= TVar;
coercions  STyp 2;

separator  Ident ",";

ROpen.     SRow  ::= "[" [Ident] "|" TVar "]";
RClos.     SRow  ::= "[" [Ident] "]";
RVar.      SRow  ::= TVar;

-- coercions  Row 1;
|]

data Typ 
  = TyLit Ident
  | TyArr Typ Row Typ
  | TyVar TVar

instance Show Typ where
  show = pShow

data Row = Row [Ident] (Maybe TVar)

instance Show Row where
  show = pShow


toSTyp :: Typ -> STyp
toSTyp t = case t of
  (TyLit v)     -> SLit v
  (TyArr a r b) -> SArr (toSTyp a) (toSRow r) (toSTyp b)
  (TyVar v)     -> SVar v

toSRow :: Row -> SRow
toSRow r = case r of 
  Row [] (Just v) -> RVar v
  Row ls (Just v) -> ROpen ls v
  Row ls Nothing  -> RClos ls

fromSTyp :: STyp -> Typ
fromSTyp s = case s of
  (SLit v)     -> TyLit v
  (SArr a r b) -> TyArr (fromSTyp a) (fromSRow r) (fromSTyp b)
  (SVar v)     -> TyVar v

fromSRow :: SRow -> Row
fromSRow r = case r of 
  RVar v     -> Row [] (Just v)
  ROpen ls v -> Row ls (Just v)
  RClos ls   -> Row ls Nothing

pShow :: Print a => a -> String
pShow = printTree

instance Print Typ where
  prt i = prt i . toSTyp

instance Print Row where
  prt i = prt i . toSRow