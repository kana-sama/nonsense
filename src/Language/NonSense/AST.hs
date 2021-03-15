{-# LANGUAGE PatternSynonyms #-}

module Language.NonSense.AST where

import NSPrelude

newtype Name = Name {unName :: Text}
  deriving stock (Eq)
  deriving newtype (IsString, Show)

type Type = Expr

type Arguments = [(Name, Type)]

data Constructor
  = Constructor Name Arguments
  deriving stock (Show)

data Declaration
  = Definition Name Arguments Type Expr
  | Inductive Name Arguments [Constructor]
  | External Name Arguments Type Text
  | Declare Name Arguments Type
  | Mutual [Declaration]
  deriving stock (Show)

data LetBinding = LetBinding Name Type Expr
  deriving stock (Eq, Show)

data Expr
  = Var Name
  | App Name [Expr]
  | Annotated Expr Type
  | Number Int
  | String Text
  | Boolean Bool
  | Interpolation [Expr]
  | Array [Expr]
  | ArrayType Type
  | Tuple [Expr]
  | TupleType [Type]
  | Wildcard Name
  | Match Expr [(Expr, Expr)]
  | Let [LetBinding] Expr
  | Top
  | Bottom
  deriving stock (Eq, Show)

pattern NumberType :: Expr
pattern NumberType = Var "number"

pattern StringType :: Expr
pattern StringType = Var "string"

pattern BooleanType :: Expr
pattern BooleanType = Var "boolean"
