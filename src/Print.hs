{-# LANGUAGE
    OverloadedStrings
#-}

module Print 
  ( Pretty(..)
  , putDocW
  , (<+>)
  , line
  , (<>)
  ) where

import qualified Data.Text  as T
import           Data.Text(Text)

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Util(putDocW)

import Grammar

instance Pretty TyVar where
  pretty (TV v) = pretty v

instance Pretty TyLit where
  pretty (TL l) = pretty l

instance Pretty Ident where
  pretty (ID i) = pretty i

instance Pretty Val where
  pretty (VInt i) = pretty i
  pretty VUnit    = "()"

instance Pretty Typ where
  pretty = prettyType 0

instance Pretty Row where
  pretty = prettyRow

instance Pretty Handler where
  pretty (Op id arg cont exp) = 
    pretty id <+> pretty arg <> "," <+> pretty cont <+> "->" <+> pretty exp
  pretty (Ret id exp) = "return" <+> pretty id <+> "->" <+> pretty exp

instance Pretty Term where
  pretty = prettyTerm 0

priority :: Int -> Int -> Doc ann -> Doc ann
priority x y = if x <= y then id else parens 

prettyType :: Int -> Typ -> Doc ann
prettyType x (TyArr a r b) = 
  priority x 0 $ prettyType 1 a <+> "->" <+> pretty r <+> prettyType 0 b
prettyType _ (TyVar v) = pretty v
prettyType _ (TyLit l) = pretty l

prettyRow :: Row -> Doc ann
prettyRow (Row [] (Just v)) = pretty v
prettyRow (Row ls (Just v)) = 
  lbracket <> sep (punctuate "," $ map pretty ls) <+> pipe <+> pretty v <> rbracket
prettyRow (Row ls Nothing)  = brackets $ sep (punctuate "," $ map pretty ls)

prettyTerm :: Int -> Term -> Doc ann
prettyTerm x (App a b) = 
  priority x 1 $ prettyTerm 2 a <+> prettyTerm 2 b
prettyTerm x (Let id b e) = 
  priority x 0 $ "let" <+> pretty id <+> "=" <+> prettyTerm 0 b <+> "in" <+> prettyTerm 0 e
prettyTerm x (Abs id b) = 
  priority x 0 $ "fn" <+> pretty id <+> "->" <+> prettyTerm 0 b
prettyTerm x (Var id) = pretty id
prettyTerm x (Lit l) = pretty l
prettyTerm x (Handle tl t hs) =
  priority x 0 $ "handle" <+> pretty tl <+> "in" <+> prettyTerm 0 t <+> "with" 
                          <+> align (vsep $ punctuate ";" $ map pretty hs)
prettyTerm x (Lift tl t) =
  priority x 0 $ "lift" <+> pretty tl <+> "in" <+> prettyTerm 2 t
prettyTerm x (Bind id a b) =
  priority x 0 $ pretty id <+> "<-" <+> prettyTerm 0 a <+> ";" <+> prettyTerm 0 b

