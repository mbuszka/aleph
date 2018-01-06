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

inParens s = "(" ++ s ++ ")"

printType = printTypeP False

printTypeP :: Bool -> Type -> String
printTypeP b (TyLit s) = s
printTypeP f (TyArr a r b) = 
  (if f then inParens else id) $ printTypeP True a ++ " -> " ++ printTypeP False r ++ " " ++ printTypeP False b
printTypeP b (TyVar (TV v)) = v
printTypeP b (TyRow ls (Just (TV v))) = init (show ls) ++ "| " ++ v ++ "]"
printTypeP b (TyRow ls Nothing) = show ls