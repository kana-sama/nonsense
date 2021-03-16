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

transpilePattern :: NS.Pattern -> TS.Expr
transpilePattern NS.PatternWildcard = TS.Unknown
transpilePattern (NS.PatternAnnotated pattern_ type_) = typed (transpileExpr type_) (transpilePattern pattern_)
transpilePattern (NS.PatternVar name) = TS.Infer (transpileName name)
transpilePattern (NS.PatternDot name) = TS.Var (transpileName name) Nothing
transpilePattern (NS.PatternConstructor name arguments) = TS.Var (transpileName name) (Just (transpilePattern <$> arguments))
transpilePattern (NS.PatternNumber number) = TS.NumberLit number
transpilePattern (NS.PatternString string) = TS.StringLit string
transpilePattern (NS.PatternBoolean boolean) = TS.BooleanLit boolean
transpilePattern (NS.PatternInterpolation parts) = TS.StringInterpolation (transpilePart <$> parts)
  where
    transpilePart (NS.PatternString string) = Left string
    transpilePart other = Right (transpilePattern other)
transpilePattern (NS.PatternArray elements) = TS.ArrayLit (transpilePattern <$> elements)
transpilePattern (NS.PatternTuple elements) = TS.ArrayLit (transpilePattern <$> elements)

transpileLetBinding :: NS.LetBinding -> TS.Expr -> TS.Expr
transpileLetBinding (NS.LetBinding pattern_ value) next =
  TS.Extends (transpileExpr value) (transpilePattern pattern_) next TS.Never

-- -- TODO: optimizer for _ and full var
transpileCaseBranch :: TS.Expr -> NS.CaseBranch -> TS.Expr -> TS.Expr
transpileCaseBranch matchedValue (NS.CaseBranch pattern_ value) next =
  TS.Extends matchedValue (transpilePattern pattern_) (transpileExpr value) next

transpileExpr :: NS.Expr -> TS.Expr
transpileExpr (NS.ExprVar name) = TS.Var (transpileName name) Nothing
transpileExpr (NS.ExprApp name arguments) = TS.Var (transpileName name) (Just (transpileExpr <$> arguments))
transpileExpr (NS.ExprAnnotated value type_) = typed (transpileExpr type_) (transpileExpr value)
transpileExpr (NS.ExprNumber number) = TS.NumberLit number
transpileExpr (NS.ExprString string) = TS.StringLit string
transpileExpr (NS.ExprBoolean boolean) = TS.BooleanLit boolean
transpileExpr (NS.ExprInterpolation parts) = TS.StringInterpolation (transpilePart <$> parts)
  where
    transpilePart (NS.ExprString string) = Left string
    transpilePart other = Right (transpileExpr other)
transpileExpr (NS.ExprArray elements) = TS.ArrayLit (transpileExpr <$> elements)
transpileExpr (NS.ExprTuple elements) = TS.ArrayLit (transpileExpr <$> elements)
transpileExpr (NS.ExprMatch value cases) = foldr (transpileCaseBranch (transpileExpr value)) TS.Never cases
transpileExpr (NS.ExprLet bindings next) = foldr transpileLetBinding (transpileExpr next) bindings
transpileExpr NS.ExprNumberType = TS.Var "number" Nothing
transpileExpr NS.ExprStringType = TS.Var "string" Nothing
transpileExpr NS.ExprBooleanType = TS.Var "boolean" Nothing
transpileExpr (NS.ExprArrayType type_) = TS.ArrayType (transpileExpr type_)
transpileExpr (NS.ExprTupleType types) = TS.ArrayLit (transpileExpr <$> types)
transpileExpr NS.ExprTop = TS.Unknown
transpileExpr NS.ExprBottom = TS.Never

transpileArguments :: [NS.Argument] -> Maybe [(TS.Ident, Maybe TS.Expr)]
transpileArguments [] = Nothing
transpileArguments arguments = Just (transpileArgument <$> arguments)
  where
    transpileArgument (NS.Argument name type_) = (transpileName name, Just (transpileExpr type_))

makeSmartConstructor :: NS.Name -> NS.Constructor -> TS.Declaration
makeSmartConstructor typeName (NS.Constructor name arguments) =
  TS.TypeDeclaration (transpileName name) (transpileArguments arguments) $
    typed (TS.Var (transpileName typeName) Nothing) $
      let tag = TS.StringLit (NS.unName name)
          payload = [TS.Var (transpileName argName) Nothing | NS.Argument argName _ <- arguments]
       in case arguments of
            [] -> tag
            _ -> TS.ArrayLit (tag : payload)

transpileConstructor :: NS.Constructor -> TS.Expr
transpileConstructor (NS.Constructor (NS.Name name) []) = TS.StringLit name
transpileConstructor (NS.Constructor (NS.Name name) arguments) =
  let payload = [transpileExpr argType | NS.Argument _ argType <- arguments]
   in TS.ArrayLit (TS.StringLit name : payload)

transpileDeclaration :: NS.Declaration -> [TS.Declaration]
transpileDeclaration (NS.Definition name args type_ value) = pure do
  TS.TypeDeclaration (transpileName name) (transpileArguments args) do
    typed (transpileExpr type_) (transpileExpr value)
transpileDeclaration (NS.Enum name cons) =
  let decl =
        TS.TypeDeclaration (transpileName name) Nothing do
          typed TS.Unknown do
            case cons of
              [] -> TS.Never
              _ -> foldr1 TS.Union (transpileConstructor <$> cons)
   in decl : fmap (makeSmartConstructor name) cons
transpileDeclaration (NS.External name args type_ body) = pure do
  TS.TypeDeclaration (transpileName name) (transpileArguments args) do
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
