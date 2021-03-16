{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Language.NonSense.Transpiler where

import qualified Language.NonSense.AST as NS
import qualified Language.TypeScript.AST as TS
import NSPrelude

encodeName :: Text -> Text
encodeName = replace "-" "_" . replace "?" "_qmark"

transpileName :: NS.Name -> TS.Ident
transpileName (NS.Name name) = TS.Ident (encodeName name)

typed :: TS.Expr -> TS.Expr -> TS.Expr
typed type_ expr = TS.App "the" [type_, expr]

transpileExpr :: NS.Expr -> TS.Expr
transpileExpr (NS.Var name) = TS.Var (transpileName name) Nothing
transpileExpr (NS.App name arguments) = TS.Var (transpileName name) (Just (transpileExpr <$> arguments))
transpileExpr (NS.Annotated value type_) = typed (transpileExpr type_) (transpileExpr value)
transpileExpr (NS.Number number) = TS.NumberLit number
transpileExpr (NS.String string) = TS.StringLit string
transpileExpr (NS.Boolean boolean) = TS.BooleanLit boolean
transpileExpr (NS.Interpolation parts) = TS.StringInterpolation (transpilePart <$> parts)
  where
    transpilePart :: NS.Expr -> Either Text TS.Expr
    transpilePart (NS.String string) = Left string
    transpilePart other = Right (transpileExpr other)
transpileExpr (NS.Array elements) = TS.ArrayLit (transpileExpr <$> elements)
transpileExpr (NS.Tuple elements) = TS.ArrayLit (transpileExpr <$> elements)
transpileExpr (NS.Match value cases) = foldr (transpileCaseBranch (transpileExpr value)) TS.Never cases
  where
    -- TODO: optimizer for _ and full var
    transpileCaseBranch :: TS.Expr -> NS.CaseBranch -> TS.Expr -> TS.Expr
    transpileCaseBranch matchedValue (NS.CaseBranch pattern_ value) next =
      TS.Extends matchedValue (transpileExpr pattern_) (transpileExpr value) next
transpileExpr (NS.Let bindings next) = foldr transpileLetBinding (transpileExpr next) bindings
  where
    transpileLetBinding :: NS.LetBinding -> TS.Expr -> TS.Expr
    transpileLetBinding (NS.LetBinding pattern_ value) next =
      TS.Extends (transpileExpr value) (transpileExpr pattern_) next TS.Never
transpileExpr NS.Top = TS.Unknown
transpileExpr NS.Bottom = TS.Never
transpileExpr NS.NumberType = TS.Var "number" Nothing
transpileExpr NS.StringType = TS.Var "string" Nothing
transpileExpr NS.BooleanType = TS.Var "boolean" Nothing
transpileExpr (NS.ArrayType elementType) = TS.ArrayType (transpileExpr elementType)
transpileExpr (NS.TupleType elementsTypes) = TS.ArrayLit (transpileExpr <$> elementsTypes)
transpileExpr NS.WildcardPattern = TS.Unknown
transpileExpr (NS.DotPattern name) = TS.Var (transpileName name) Nothing
transpileExpr (NS.InferPattern name) = TS.Infer (transpileName name)
transpileExpr NS.Meta = error "impossible: meta"

transpileArguments :: [NS.Argument] -> Maybe [(TS.Ident, Maybe TS.Expr)]
transpileArguments [] = Nothing
transpileArguments arguments = Just (transpileArgument <$> arguments)
  where
    transpileArgument (NS.Argument name type_) = (transpileName name, Just (transpileExpr type_))

transpileConstructor :: [NS.Argument] -> NS.Constructor -> TS.Expr
transpileConstructor typeArguments constructor =
  case arguments of
    [] -> TS.StringLit constructor.name.value
    _ -> TS.ArrayLit (TS.StringLit constructor.name.value : payload)
  where
    arguments = typeArguments <> constructor.arguments
    payload = [transpileExpr a.type_ | a <- arguments]

makeSmartConstructor :: NS.Name -> [NS.Argument] -> NS.Constructor -> TS.Declaration
makeSmartConstructor typeName typeArguments constructor =
  TS.TypeDeclaration (transpileName constructor.name) (transpileArguments arguments) do
    typed constructorType do
      case arguments of
        [] -> constructorTag
        _ -> TS.ArrayLit (constructorTag : constructorPayload)
  where
    arguments = typeArguments <> constructor.arguments
    constructorTag = TS.StringLit constructor.name.value
    constructorPayload =
      [TS.Var (transpileName a.name) Nothing | a <- arguments]

    constructorType =
      case typeArguments of
        [] -> TS.Var (transpileName typeName) Nothing
        _ -> TS.Var (transpileName typeName) do
          Just [TS.Var (transpileName a.name) Nothing | a <- typeArguments]

transpileDeclaration :: NS.Declaration -> [TS.Declaration]
transpileDeclaration (NS.Definition name arguments type_ value) = pure do
  TS.TypeDeclaration (transpileName name) (transpileArguments arguments) do
    typed (transpileExpr type_) (transpileExpr value)
transpileDeclaration (NS.Enum name constructors arguments) =
  let decl =
        TS.TypeDeclaration (transpileName name) (transpileArguments arguments) do
          typed TS.Unknown do
            case constructors of
              [] -> TS.Never
              _ -> foldr1 TS.Union (transpileConstructor arguments <$> constructors)
   in decl : fmap (makeSmartConstructor name arguments) constructors
transpileDeclaration (NS.External name arguments type_ body) = pure do
  TS.TypeDeclaration (transpileName name) (transpileArguments arguments) do
    typed (transpileExpr type_) (TS.External body)
transpileDeclaration (NS.Declare _ _ _) = []
transpileDeclaration (NS.Mutual declarations) =
  foldMap transpileDeclaration declarations

builtin :: [TS.Declaration]
builtin = [the, plus]
  where
    the = TS.TypeDeclaration "the" (Just [("a", Just TS.Unknown), ("b", Just (TS.Var "a" Nothing))]) do
      TS.Var "b" Nothing

    plusmap = TS.ArrayLit [TS.ArrayLit [TS.NumberLit b | b <- [a .. 50]] | a <- [0 .. 50]]
    plus = TS.TypeDeclaration "plus" (Just [("a", Just (TS.Var "number" Nothing)), ("b", Just (TS.Var "number" Nothing))]) do
      TS.Lookup (TS.Lookup plusmap (TS.Var "a" Nothing)) (TS.Var "b" Nothing)

transpileModule :: [NS.Declaration] -> TS.Module
transpileModule decls = TS.Module (builtin <> foldMap transpileDeclaration decls)
