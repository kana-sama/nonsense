module Language.NonSense.AST where

import NSPrelude

newtype Name = Name {unName :: Text}
  deriving stock (Show, Eq, Data)
  deriving newtype (IsString)

type Type = Expr

type Arguments = [(Name, Type)]

data Constructor
  = Constructor Name Arguments
  deriving stock (Show)

data Declaration
  = Definition Name Arguments Type Expr
  | Inductive Name Arguments [Constructor]
  | External Name Arguments Type Text
  deriving stock (Show)

data LetBinding = LetBinding Name Type Expr
  deriving stock (Eq, Show, Data)

newtype Wildcard = MkWildcard {unWildcard :: Name}
  deriving stock (Show, Eq, Data)
  deriving newtype (IsString)

data Expr
  = Var Name
  | App Name [Expr]
  | Number Int
  | String Text
  | Array [Expr]
  | Object [(Text, Expr)]
  | Wildcard Wildcard
  | Match Expr [(Expr, Expr)]
  | Let [LetBinding] Expr
  | ArrayType Type
  | U
  deriving stock (Eq, Show, Data)
