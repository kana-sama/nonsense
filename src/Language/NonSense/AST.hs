module Language.NonSense.AST where

import NSPrelude

newtype Name = Name {unName :: Text}
  deriving stock (Show)
  deriving newtype (IsString)

type Arguments = [(Name, Expr)]

data Constructor
  = Constructor Name Arguments
  deriving (Show)

data Declaration
  = Definition Name Arguments Expr Expr
  | Inductive Name Arguments [Constructor]
  | External Name Arguments (Maybe Expr) Text
  deriving stock (Show)

data Expr
  = Var Name
  | App Name [Expr]
  | Number Int
  | String Text
  | Array [Expr]
  | Object [(Text, Expr)]
  | Annotation Expr Expr
  | Wildcard Name
  | Match Expr [(Expr, Expr)]
  | Let Name Expr Expr Expr
  | U
  deriving stock (Show)
