module Common where

import qualified Text.Parsec as Par


data Error
  = ParseError Par.ParseError
  | RenameError String
  deriving Show