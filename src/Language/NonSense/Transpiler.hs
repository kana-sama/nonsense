module Language.NonSense.Transpiler where

import qualified Language.NonSense.AST as NS
import qualified Language.TypeScript.AST as TS
import NSPrelude

encodeName :: Text -> Text
encodeName = replace "-" "_" . replace "?" "_qmark"

transpileName :: NS.Name -> TS.Ident
transpileName (NS.Name name) = TS.Ident (encodeName name)

typed :: NS.Type -> TS.Expr -> TS.Expr
typed a b = TS.App "the" [transpileExpr a, b]

transpileLetBinding :: NS.LetBinding -> TS.Expr -> TS.Expr
transpileLetBinding (NS.LetBinding name type_ value) next =
  TS.Extends (transpileExpr value) (typed type_ (TS.Infer (transpileName name))) next TS.Never

transpileExpr :: NS.Expr -> TS.Expr
transpileExpr (NS.Var name) = TS.Var (transpileName name) Nothing
transpileExpr (NS.App name args) = TS.Var (transpileName name) (Just (transpileExpr <$> args))
transpileExpr (NS.Number x) = TS.NumberLit x
transpileExpr (NS.String x) = TS.StringLit x
transpileExpr (NS.Array xs) = TS.ArrayLit (transpileExpr <$> xs)
transpileExpr (NS.Object kvs) = TS.ObjectLit [(k, transpileExpr v) | (k, v) <- kvs]
transpileExpr (NS.Wildcard name) = TS.Infer (transpileName (NS.unWildcard name))
transpileExpr (NS.Match e cases) =
  foldr (\(pat, expr) -> TS.Extends (transpileExpr e) (transpileExpr pat) (transpileExpr expr)) TS.Never cases
transpileExpr (NS.Let bindings next) = foldr transpileLetBinding (transpileExpr next) bindings
transpileExpr (NS.ArrayType elem) = TS.ArrayType (transpileExpr elem)
transpileExpr NS.U = TS.Unknown

transpileArguments :: NS.Arguments -> Maybe [(TS.Ident, Maybe TS.Expr)]
transpileArguments [] = Nothing
transpileArguments args = Just (transpileArgument <$> args)

transpileArgument :: (NS.Name, NS.Expr) -> (TS.Ident, Maybe TS.Expr)
transpileArgument (name, type_) = (transpileName name, Just (transpileExpr type_))

transpileDeclaration :: NS.Declaration -> [TS.Declaration]
transpileDeclaration (NS.Definition name args type_ value) =
  pure . TS.TypeDeclaration (transpileName name) (transpileArguments args) $
    typed type_ (transpileExpr value)
transpileDeclaration (NS.Inductive name args cons) =
  let decl =
        TS.TypeDeclaration (transpileName name) (transpileArguments args) $
          typed NS.U $
            foldr TS.Union TS.Never (transpileConstructor <$> cons)
   in decl : fmap (makeSmartConstructor name) cons
transpileDeclaration (NS.External name args type_ body) =
  pure . TS.TypeDeclaration (transpileName name) (transpileArguments args) $
    typed type_ (TS.External body)

makeSmartConstructor :: NS.Name -> NS.Constructor -> TS.Declaration
makeSmartConstructor typeName (NS.Constructor (NS.Name name) args) =
  TS.TypeDeclaration (TS.Ident name) (transpileArguments args) $
    typed (NS.Var typeName) $
      TS.ObjectLit
        [ ("tag", TS.StringLit name),
          ("values", TS.ObjectLit [(encodeName (NS.unName name), TS.Var (transpileName name) Nothing) | (name, _) <- args])
        ]

transpileConstructor :: NS.Constructor -> TS.Expr
transpileConstructor (NS.Constructor (NS.Name name) args) =
  TS.ObjectLit
    [ ("tag", TS.StringLit name),
      ("values", TS.ObjectLit [(name, transpileExpr value) | (NS.Name name, value) <- args])
    ]

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
