module Language.NonSense.Parser where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Language.NonSense.AST
import NSPrelude
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type ConstructorName = Name

newtype Parser a = Parser {unParser :: StateT (Set ConstructorName) (Parsec Void Text) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus, MonadState (Set ConstructorName), MonadParsec Void Text)

declareConstructor :: ConstructorName -> Parser ()
declareConstructor name = modify' (Set.insert name)

isConstructor :: Name -> Parser Bool
isConstructor name = gets (Set.member name)

-- base

keywords :: [Text]
keywords =
  [ "array",
    "tuple",
    "let",
    "in",
    "def",
    "inductive",
    "external",
    "declare",
    "mutual",
    "type",
    "match",
    "with",
    "end",
    "extends",
    "keyof",
    "typeof",
    "top",
    "bottom",
    "number",
    "string",
    "boolean",
    "_"
  ]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

numberLiteral :: Parser Int
numberLiteral = L.signed sc (lexeme L.decimal)

stringLiteral :: Parser Text
stringLiteral =
  lexeme $
    choice
      [ char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')),
        char '\'' *> (pack <$> manyTill L.charLiteral (char '\''))
      ]

booleanLiteral :: Parser Bool
booleanLiteral =
  choice
    [ keyword "true" $> True,
      keyword "false" $> False
    ]

keyword :: Text -> Parser Text
keyword keyword = try $ lexeme (string keyword <* notFollowedBy alphaNumChar)

parens :: Text -> Parser a -> Parser a
parens "()" = between (symbol "(") (symbol ")")
parens "[]" = between (symbol "[") (symbol "]")
parens "{}" = between (symbol "{") (symbol "}")
parens "<>" = between (symbol "<") (symbol ">")
parens _ = error "invalid paren type"

name :: Parser Name
name = try do
  n <- lexeme $ Name . pack <$> liftA2 (:) letterChar (many (alphaNumChar <|> oneOf ['_', '-', '?']))
  when (unName n `elem` keywords) do
    failure (Just (Label (NonEmpty.fromList "name can't be keyword"))) mempty
  pure n

comma :: Parser ()
comma = void (symbol ",")

-- domain parsers

pattern' :: Parser Pattern
pattern' = choice [wildrcard, annotated, dot, number, string, boolean, interpolation, array, tuple, constructor, var] <?> "pattern"
  where
    items = pattern' `sepBy` comma
    wildrcard = keyword "_" $> PatternWildcard
    annotated = try (parens "()" (PatternAnnotated <$> pattern' <*> (symbol ":" *> expr)))
    var = do
      varName <- name
      varIsConstructor <- isConstructor varName
      if varIsConstructor
        then pure (PatternDot varName)
        else pure (PatternVar varName)
    dot = char '.' *> name <&> PatternDot
    constructor = try (PatternConstructor <$> name <*> parens "()" items)
    number = numberLiteral <&> PatternNumber
    string = stringLiteral <&> PatternString
    boolean = booleanLiteral <&> PatternBoolean
    interpolation = parens "<>" items <&> PatternInterpolation
    array = parens "[]" items <&> PatternArray
    tuple = parens "()" items <&> PatternTuple

expr :: Parser Expr
expr =
  (<?> "expr") . choice . concat $
    [ [app, annotated],
      [number, string, boolean],
      [interpolation, array, tuple],
      [match, let_],
      [numberType, stringType, booleanType, arrayType, tupleType],
      [topType, bottomType],
      [var]
    ]
  where
    items = expr `sepBy` comma
    var = name <&> ExprVar
    app = try (ExprApp <$> name <*> parens "()" items)
    annotated = try (parens "()" (ExprAnnotated <$> expr <*> (symbol ":" *> expr)))
    number = numberLiteral <&> ExprNumber
    string = stringLiteral <&> ExprString
    boolean = booleanLiteral <&> ExprBoolean
    interpolation = parens "<>" items <&> ExprInterpolation
    array = parens "[]" items <&> ExprArray
    tuple = parens "()" items <&> ExprTuple
    caseBranch = CaseBranch <$> (symbol "|" *> pattern') <*> (symbol ":=" *> expr)
    match = ExprMatch <$> (keyword "match" *> expr <* keyword "with") <*> (caseBranch `manyTill` keyword "end")
    letBinding = LetBinding <$> pattern' <*> (symbol ":=" *> expr)
    let_ = ExprLet <$> (keyword "let" *> many letBinding) <*> (keyword "in" *> expr)
    numberType = keyword "number" $> ExprNumberType
    stringType = keyword "string" $> ExprStringType
    booleanType = keyword "boolean" $> ExprBooleanType
    arrayType = keyword "array" *> parens "()" expr <&> ExprArrayType
    tupleType = keyword "tuple" *> parens "()" items <&> ExprTupleType
    topType = keyword "top" $> ExprTop
    bottomType = keyword "bottom" $> ExprBottom

arguments :: Parser [Argument]
arguments = concat <$> parens "()" (argument `sepBy` comma)
  where
    argument = do
      argNames <- some name
      argType <- keyword ":" *> expr
      pure [Argument argName argType | argName <- argNames]

definition :: Parser Declaration
definition = do
  keyword "def"
  name_ <- name
  args_ <- option [] arguments
  type_ <- keyword ":" *> expr
  body_ <- keyword ":=" *> expr
  pure (Definition name_ args_ type_ body_)

enum :: Parser Declaration
enum = do
  keyword "enum"
  name_ <- name
  cons_ <- many constructor
  pure (Enum name_ cons_)
  where
    constructor = do
      constructorName <- symbol "|" *> name
      constructorArguments <- option [] arguments
      declareConstructor constructorName
      pure (Constructor constructorName constructorArguments)

external :: Parser Declaration
external = do
  keyword "external"
  name_ <- name
  args_ <- option [] arguments
  type_ <- symbol ":" *> expr
  body_ <- symbol ":=" *> stringLiteral
  pure (External name_ args_ type_ body_)

declare :: Parser Declaration
declare = do
  keyword "declare"
  name_ <- name
  args_ <- option [] arguments
  type_ <- symbol ":" *> expr
  pure (Declare name_ args_ type_)

mutual :: Parser Declaration
mutual = do
  keyword "mutual"
  Mutual <$> declaration `manyTill` keyword "end"

declaration :: Parser Declaration
declaration = choice [definition, enum, external, declare, mutual]

module_ :: Parser [Declaration]
module_ = sc *> many declaration <* eof

parseDeclarations :: FilePath -> Text -> Either Text [Declaration]
parseDeclarations path source =
  case runParser (evalStateT (unParser module_) Set.empty) path source of
    Right decls -> Right decls
    Left errors -> Left (pack (errorBundlePretty errors))
