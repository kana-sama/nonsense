{-# LANGUAGE PatternSynonyms #-}

module Language.TypeScript.AST where

import NSPrelude

newtype Ident = Ident Text
  deriving newtype (IsString)

newtype Module = Module [Declaration]

data Declaration = TypeDeclaration Ident (Maybe [(Ident, Maybe Expr)]) Expr

data Expr
  = Var Ident (Maybe [Expr])
  | NumberLit Int
  | StringLit Text
  | BooleanLit Bool
  | ArrayLit [Expr]
  | ObjectLit [(Text, Expr)]
  | StringInterpolation [Either Text Expr]
  | Infer Ident
  | Extends Expr Expr Expr Expr
  | Union Expr Expr
  | Intersection Expr Expr
  | Never
  | Unknown
  | Any
  | Parens Expr
  | Lookup Expr Expr
  | KeyOf Expr
  | ArrayType Expr
  | External Text

instance IsString Expr where
  fromString name = Var (Ident (pack name)) Nothing

pattern App name args = Var name (Just args)
