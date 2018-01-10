{-# LANGUAGE
    OverloadedStrings
#-}

module Print 
  ( Pretty(..)
  , putDocW
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

instance Pretty Typ where
  pretty = prettyType 0

instance Pretty Row where
  pretty = prettyRow

prettyType :: Int -> Typ -> Doc ann
prettyType x (TyArr a r b) = 
  (if x < 1 then id else parens) $ (prettyType 1 a <+> "->" <+> pretty r <+> prettyType 0 b)
prettyType _ (TyVar v) = pretty v
prettyType _ (TyLit l) = pretty l

prettyRow :: Row -> Doc ann
prettyRow (Row [] (Just v)) = pretty v
prettyRow (Row ls (Just v)) = 
  lbracket <> sep (punctuate "," $ map pretty ls) <+> pipe <+> pretty v <> rbracket
prettyRow (Row ls Nothing)  = brackets $ sep (punctuate "," $ map pretty ls)