module Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TyLit String
  | TyArr Type Type Type
  | TyVar TVar
  | TyRow [String] (Maybe TVar)
  deriving (Show)

type Constraint = (Type, Type)

data Kind = KRow | KType
  deriving (Show)

data Scheme = Scheme [TVar] Type
  deriving (Show)

printType :: Type -> String
printType (TyLit s) = s
printType (TyArr a r b) = printType a ++ " -> " ++ printType r ++ " (" ++ printType b ++ ")"
printType (TyVar (TV v)) = v
printType (TyRow ls (Just (TV v))) = init (show ls) ++ "| " ++ v ++ "]"
printType (TyRow ls Nothing) = show ls