{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Language.NonSense.AST where

import NSPrelude
import qualified Prelude

newtype Name = Name {value :: Text}
  deriving stock (Eq, Ord)
  deriving newtype (IsString, Show)

type Type = Expr

type Pattern = Expr

data Argument = Argument {name :: Name, type_ :: Type}
  deriving stock (Eq)

instance Show Argument where
  show arg = unpack (arg.name.value <> ":" <> show arg.type_)

data Constructor = Constructor {name :: Name, arguments :: [Argument]}
  deriving stock (Show, Eq)

data Declaration
  = Definition Name [Argument] Type Expr
  | Enum Name [Constructor] [Argument]
  | External Name [Argument] Type Text
  | Declare Name [Argument] Type
  | Mutual [Declaration]
  deriving stock (Show, Eq)

data LetBinding = LetBinding {pattern_ :: Pattern, value :: Expr}
  deriving stock (Eq, Show)

data CaseBranch = CaseBranch {pattern_ :: Pattern, value :: Expr}
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
  | Tuple [Expr]
  | Match Expr [CaseBranch]
  | Let [LetBinding] Expr
  | Top
  | Bottom
  | NumberType
  | StringType
  | BooleanType
  | ArrayType Type
  | TupleType [Type]
  | WildcardPattern
  | DotPattern Name
  | InferPattern Name
  | Meta
  deriving stock (Show)

instance Eq Expr where
  Meta == _ = True
  _ == Meta = True
  Var a == Var a' = a == a'
  App a b == App a' b' = a == a' && b == b'
  Annotated a b == Annotated a' b' = a == a' && b == b'
  Number a == Number a' = a == a'
  String a == String a' = a == a'
  Boolean a == Boolean a' = a == a'
  Interpolation a == Interpolation a' = a == a'
  Array a == Array a' = a == a'
  Tuple a == Tuple a' = a == a'
  Match a b == Match a' b' = a == a' && b == b'
  Let a b == Let a' b' = a == a' && b == b'
  Top == Top = True
  Bottom == Bottom = True
  NumberType == NumberType = True
  StringType == StringType = True
  BooleanType == BooleanType = True
  ArrayType a == ArrayType a' = a == a'
  TupleType a == TupleType a' = a == a'
  WildcardPattern == WildcardPattern = True
  DotPattern a == DotPattern a' = a == a'
  InferPattern a == InferPattern a' = a == a'
  _ == _ = False
