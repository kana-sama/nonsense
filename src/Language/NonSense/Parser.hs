{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Language.NonSense.Parser where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Language.NonSense.AST
import NSPrelude
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type ConstructorName = Name

data ParserMode = ValueMode | PatternMode
  deriving (Eq)

newtype Parser a = Parser {unParser :: ReaderT ParserMode (StateT (Set ConstructorName) (Parsec Void Text)) a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus)
  deriving newtype (MonadState (Set ConstructorName))
  deriving newtype (MonadReader ParserMode)
  deriving newtype (MonadParsec Void Text)

getParserMode :: Parser ParserMode
getParserMode = ask

declareConstructor :: ConstructorName -> Parser ()
declareConstructor nameParser = modify' (Set.insert nameParser)

isConstructor :: Name -> Parser Bool
isConstructor nameParser = gets (Set.member nameParser)

withPatternMode :: Parser a -> Parser a
withPatternMode = local (const PatternMode)

guardMode :: ParserMode -> Parser ()
guardMode mode = do
  currentMode <- getParserMode
  guard (mode == currentMode)

-- base

keywords :: [Name]
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

nameParser :: Parser Name
nameParser = try do
  name <- lexeme $ Name . pack <$> liftA2 (:) letterChar (many (alphaNumChar <|> oneOf ['_', '-', '?']))
  when (name `elem` keywords) do
    failure (Just (Label (NonEmpty.fromList "name can't be keyword"))) mempty
  pure name

comma :: Parser ()
comma = void (symbol ",")

-- domain parsers

var :: Parser Expr
var = try do
  guardMode ValueMode
  Var <$> nameParser

app :: Parser Expr
app = try (App <$> nameParser <*> parens "()" exprs)

annotated :: Parser Expr
annotated = try (parens "()" (Annotated <$> expr <*> (symbol ":" *> expr)))

number :: Parser Expr
number = numberLiteral <&> Number

string_ :: Parser Expr
string_ = stringLiteral <&> String

boolean :: Parser Expr
boolean = booleanLiteral <&> Boolean

interpolation :: Parser Expr
interpolation = parens "<>" exprs <&> Interpolation

array :: Parser Expr
array = parens "[]" exprs <&> Array

tuple :: Parser Expr
tuple = parens "()" exprs <&> Tuple

caseBranch :: Parser CaseBranch
caseBranch = CaseBranch <$> (symbol "|" *> withPatternMode expr) <*> (symbol ":=" *> expr)

match_ :: Parser Expr
match_ = try do
  guardMode ValueMode
  Match <$> (keyword "match" *> expr <* keyword "with") <*> (caseBranch `manyTill` keyword "end")

letBinding :: Parser LetBinding
letBinding = LetBinding <$> (withPatternMode expr) <*> (symbol ":=" *> expr)

let_ :: Parser Expr
let_ = try do
  guardMode ValueMode
  Let <$> (keyword "let" *> many letBinding) <*> (keyword "in" *> expr)

numberType :: Parser Expr
numberType = keyword "number" $> NumberType

stringType :: Parser Expr
stringType = keyword "string" $> StringType

booleanType :: Parser Expr
booleanType = keyword "boolean" $> BooleanType

arrayType :: Parser Expr
arrayType = keyword "array" *> parens "()" expr <&> ArrayType

tupleType :: Parser Expr
tupleType = keyword "tuple" *> parens "()" exprs <&> TupleType

top :: Parser Expr
top = keyword "top" $> Top

bottom :: Parser Expr
bottom = keyword "bottom" $> Bottom

wildrcardPattern :: Parser Expr
wildrcardPattern = try do
  guardMode PatternMode
  keyword "_" $> WildcardPattern

dotPattern :: Parser Expr
dotPattern = try do
  guardMode PatternMode
  char '.' *> nameParser <&> DotPattern

namePattern :: Parser Expr
namePattern = try do
  guardMode PatternMode
  varName <- nameParser
  varIsConstructor <- isConstructor varName
  if varIsConstructor
    then pure (DotPattern varName)
    else pure (InferPattern varName)

expr :: Parser Expr
expr =
  (<?> "expr") . choice . concat $
    [ [annotated],
      [number, string_, boolean, interpolation, array, tuple],
      [match_, let_],
      [top, bottom, numberType, stringType, booleanType, arrayType, tupleType],
      [app, var],
      [wildrcardPattern, dotPattern, namePattern]
    ]

exprs :: Parser [Expr]
exprs = expr `sepBy` comma

argumentsParser :: Parser [Argument]
argumentsParser = concat <$> parens "()" (argument `sepBy` comma)
  where
    argument = do
      argNames <- some nameParser
      argType <- keyword ":" *> expr
      pure [Argument argName argType | argName <- argNames]

definition :: Parser Declaration
definition = do
  keyword "def"
  name_ <- nameParser
  args_ <- option [] argumentsParser
  type_ <- keyword ":" *> expr
  body_ <- keyword ":=" *> expr
  pure (Definition name_ args_ type_ body_)

enum :: Parser Declaration
enum = do
  keyword "enum"
  name_ <- nameParser
  args_ <- option [] argumentsParser
  cons_ <- keyword ":=" *> many constructor
  declareConstructor name_
  pure (Enum name_ cons_ args_)
  where
    constructor = do
      constructorName <- symbol "|" *> nameParser
      constructorArguments <- option [] argumentsParser
      declareConstructor constructorName
      pure (Constructor constructorName constructorArguments)

external :: Parser Declaration
external = do
  keyword "external"
  name_ <- nameParser
  args_ <- option [] argumentsParser
  type_ <- symbol ":" *> expr
  body_ <- symbol ":=" *> stringLiteral
  pure (External name_ args_ type_ body_)

declare :: Parser Declaration
declare = do
  keyword "declare"
  name_ <- nameParser
  args_ <- option [] argumentsParser
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
  case runParser (evalStateT (runReaderT (unParser module_) ValueMode) Set.empty) path source of
    Right decls -> Right decls
    Left errors -> Left (pack (errorBundlePretty errors))
