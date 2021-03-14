module Language.TypeScript.PrettyPrinter where

import Language.TypeScript.AST
import NSPrelude

class Pretty a where
  pretty :: a -> Text

instance Pretty Ident where
  pretty (Ident a) = a

instance Pretty Expr where
  pretty (Var name Nothing) = pretty name
  pretty (Var name (Just args)) = pretty name <> "<" <> intercalate ", " (pretty <$> args) <> ">"
  pretty (NumberLit num) = show num
  pretty (StringLit str) = show str
  pretty (ArrayLit values) = "[" <> intercalate ", " (pretty <$> values) <> "]"
  pretty (ObjectLit kvs) = "{" <> intercalate ", " [show k <> ": " <> pretty v | (k, v) <- kvs] <> "}"
  pretty (StringInterpolation elems) = "\"" <> foldMap prettyElem elems <> "\""
    where
      prettyElem (Left lit) = lit
      prettyElem (Right expr) = "${" <> pretty expr <> "}"
  pretty (Infer var) = "infer " <> pretty var
  pretty (Extends a b then_ else_) = pretty a <> " extends " <> pretty b <> " ? " <> pretty then_ <> " : " <> pretty else_
  pretty (Union a b) = pretty a <> " | " <> pretty b
  pretty (Intersection a b) = pretty a <> " & " <> pretty b
  pretty Never = "never"
  pretty Unknown = "unknown"
  pretty Any = "any"
  pretty (Parens expr) = "(" <> pretty expr <> ")"
  pretty (Lookup a b) = pretty a <> "[" <> pretty b <> "]"
  pretty (KeyOf a) = "keyof " <> pretty a
  pretty (ArrayType a) = pretty a <> "[]"
  pretty (External src) = src

instance Pretty Declaration where
  pretty (TypeDeclaration name args body) = "type " <> pretty name <> prettyArgs args <> " = " <> pretty body
    where
      prettyArgs Nothing = ""
      prettyArgs (Just args) = "<" <> intercalate ", " (prettyArg <$> args) <> ">"
      prettyArg (name, Nothing) = pretty name
      prettyArg (name, Just extends) = pretty name <> " extends " <> pretty extends

instance Pretty Module where
  pretty (Module declarations) = unlines (pretty <$> declarations)
