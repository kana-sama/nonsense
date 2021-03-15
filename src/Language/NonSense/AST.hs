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
  deriving stock (Show)

data LetBinding = LetBinding Name Type Expr
  deriving stock (Eq, Show)

data Expr
  = Var Name
  | App Name [Expr]
  | Number Int
  | String Text
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

pattern NumberType = Var "number"

pattern StringType = Var "string"
