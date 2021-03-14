module Language.NonSense.Transpiler where

import qualified Language.NonSense.AST as NS
import qualified Language.TypeScript.AST as TS
import NSPrelude

encodeName :: Text -> Text
encodeName = replace "-" "_" . replace "?" "_qmark"

transpileName :: NS.Name -> TS.Ident
transpileName (NS.Name name) = TS.Ident (encodeName name)

typed :: Maybe NS.Expr -> TS.Expr -> TS.Expr
typed (Just a) b = TS.App "the" [transpileExpr a, b]
typed Nothing b = b

transpileExpr :: NS.Expr -> TS.Expr
transpileExpr (NS.Var name) = TS.Var (transpileName name) Nothing
transpileExpr (NS.App name args) = TS.Var (transpileName name) (Just (transpileExpr <$> args))
transpileExpr (NS.Number x) = TS.NumberLit x
transpileExpr (NS.String x) = TS.StringLit x
transpileExpr (NS.Array xs) = TS.ArrayLit (transpileExpr <$> xs)
transpileExpr (NS.Object kvs) = TS.ObjectLit [(k, transpileExpr v) | (k, v) <- kvs]
transpileExpr (NS.Annotation a b) = typed (Just a) (transpileExpr b)
transpileExpr (NS.Wildcard name) = TS.Infer (transpileName name)
transpileExpr (NS.Match e cases) =
  foldr (\(pat, expr) -> TS.Extends (transpileExpr e) (transpileExpr pat) (transpileExpr expr)) TS.Never cases
transpileExpr (NS.Let name value next) =
  TS.Extends (transpileExpr value) (TS.Infer (transpileName name)) (transpileExpr next) TS.Never
transpileExpr NS.U = TS.Unknown

transpileArguments :: NS.Arguments -> Maybe [(TS.Ident, Maybe TS.Expr)]
transpileArguments [] = Nothing
transpileArguments args = Just (transpileArgument <$> args)

transpileArgument :: (NS.Name, NS.Expr) -> (TS.Ident, Maybe TS.Expr)
transpileArgument (name, type_) = (transpileName name, Just (transpileExpr type_))

transpileDeclaration :: NS.Declaration -> [TS.Declaration]
transpileDeclaration (NS.Definition name args type_ value) =
  pure . TS.TypeDeclaration (transpileName name) (transpileArguments args) $
    typed (Just type_) (transpileExpr value)
transpileDeclaration (NS.Inductive name args cons) =
  let decl =
        TS.TypeDeclaration (transpileName name) (transpileArguments args) $
          typed (Just NS.U) $
            foldr TS.Union TS.Never (transpileConstructor <$> cons)
   in decl : fmap (makeSmartConstructor name) cons
transpileDeclaration (NS.External name args type_ body) =
  pure . TS.TypeDeclaration (transpileName name) (transpileArguments args) $
    typed type_ (TS.External body)

makeSmartConstructor :: NS.Name -> NS.Constructor -> TS.Declaration
makeSmartConstructor typeName (NS.Constructor (NS.Name name) args) =
  TS.TypeDeclaration (TS.Ident name) (transpileArguments args) $
    typed (Just (NS.Var typeName)) $
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
