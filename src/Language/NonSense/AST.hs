{-# LANGUAGE PatternSynonyms #-}

module Language.NonSense.AST where

import NSPrelude

newtype Name = Name {unName :: Text}
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

type Type = Expr

data Argument = Argument Name Type
  deriving stock (Show, Eq)

data Constructor = Constructor Name [Argument]
  deriving stock (Show, Eq)

data Declaration
  = Definition Name [Argument] Type Expr
  | Enum Name [Constructor]
  | External Name [Argument] Type Text
  | Declare Name [Argument] Type
  | Mutual [Declaration]
  deriving stock (Show, Eq)

data LetBinding = LetBinding Pattern Expr
  deriving stock (Eq, Show)

data CaseBranch = CaseBranch Pattern Expr
  deriving stock (Eq, Show)

data Expr
  = ExprVar Name
  | ExprApp Name [Expr]
  | ExprAnnotated Expr Type
  | ExprNumber Int
  | ExprString Text
  | ExprBoolean Bool
  | ExprInterpolation [Expr]
  | ExprArray [Expr]
  | ExprTuple [Expr]
  | ExprMatch Expr [CaseBranch]
  | ExprLet [LetBinding] Expr
  | ExprNumberType
  | ExprStringType
  | ExprBooleanType
  | ExprArrayType Type
  | ExprTupleType [Type]
  | ExprTop
  | ExprBottom
  deriving stock (Eq, Show)

data Pattern
  = PatternWildcard
  | PatternAnnotated Pattern Expr
  | PatternVar Name
  | PatternDot Name
  | PatternConstructor Name [Pattern]
  | PatternNumber Int
  | PatternString Text
  | PatternBoolean Bool
  | PatternInterpolation [Pattern]
  | PatternArray [Pattern]
  | PatternTuple [Pattern]
  deriving stock (Eq, Show)
