module Language.NonSense.Parser where

import qualified Data.List.NonEmpty as NonEmpty
import Language.NonSense.AST
import NSPrelude hiding (many, some)
import Text.Megaparsec hiding (match)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Parser a = Parser {unParser :: Parsec Void Text a}
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadPlus, MonadParsec Void Text)

keywords :: [Text]
keywords =
  [ "array",
    "tuple",
    "in",
    "let",
    "def",
    "inductive",
    "external",
    "type",
    "match",
    "with",
    "extends",
    "keyof",
    "typeof",
    "top",
    "bottom"
  ]

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

stringLiteral :: Parser Text
stringLiteral =
  lexeme $
    choice
      [ char '\"' *> (pack <$> manyTill L.charLiteral (char '\"')),
        char '\'' *> (pack <$> manyTill L.charLiteral (char '\''))
      ]

numberLiteral :: Parser Int
numberLiteral = L.signed sc (lexeme L.decimal)

keyword :: Text -> Parser Text
keyword keyword = try $ lexeme (string keyword <* notFollowedBy alphaNumChar)

name :: Parser Name
name = try do
  n <- lexeme $ Name . pack <$> liftA2 (:) letterChar (many (alphaNumChar <|> oneOf ['_', '-', '?']))
  when (unName n `elem` keywords) do
    failure (Just (Label (NonEmpty.fromList "name can't be keyword"))) mempty
  pure n

parens :: Text -> Parser a -> Parser a
parens "()" = between (symbol "(") (symbol ")")
parens "[]" = between (symbol "[") (symbol "]")
parens "{}" = between (symbol "{") (symbol "}")
parens _ = error "invalid paren type"

comma :: Parser ()
comma = void (symbol ",")

array :: Parser Expr
array = Array <$> parens "[]" (expression `sepBy` comma)

arrayType :: Parser Expr
arrayType = ArrayType <$> (keyword "array" *> parens "()" expression)

tuple :: Parser Expr
tuple = Tuple <$> parens "()" (expression `sepBy` comma)

tupleType :: Parser Expr
tupleType = TupleType <$> (keyword "tuple" *> parens "()" (expression `sepBy` comma))

app :: Parser Expr
app = try do
  function <- name
  arguments <- parens "()" (expression `sepBy` comma)
  pure (App function arguments)

top :: Parser Expr
top = Top <$ keyword "top"

bottom :: Parser Expr
bottom = Bottom <$ keyword "bottom"

match :: Parser Expr
match = do
  keyword "match"
  expr <- expression
  keyword "with"
  cases <- many do
    symbol "|"
    pat <- expression
    symbol "=>"
    val <- expression
    pure (pat, val)
  pure (Match expr cases)

letBinding :: Parser LetBinding
letBinding = do
  name_ <- name
  type_ <- symbol ":" *> expression
  value <- symbol "=>" *> expression
  pure (LetBinding name_ type_ value)

let_ :: Parser Expr
let_ = do
  keyword "let"
  bindings <- many letBinding
  next <- keyword "in" *> expression
  pure (Let bindings next)

wildrcard :: Parser Expr
wildrcard = Wildcard <$> (char '?' *> name)

expression :: Parser Expr
expression =
  choice
    [ String <$> stringLiteral,
      Number <$> numberLiteral,
      array,
      tuple,
      arrayType,
      tupleType,
      app,
      wildrcard,
      match,
      let_,
      top,
      bottom,
      Var <$> name
    ]

arguments :: Parser Arguments
arguments = mconcat <$> parens "()" (argument `sepBy` comma)
  where
    argument = do
      argNames <- some name
      symbol ":"
      argType <- expression
      pure [(argName, argType) | argName <- argNames]

constructor :: Parser Constructor
constructor = do
  symbol "|"
  conName <- name
  conArgs <- option [] arguments
  pure (Constructor conName conArgs)

commonDeclaretion :: Text -> Parser (Name, Arguments, Type)
commonDeclaretion kv = do
  keyword kv
  name_ <- name
  args_ <- option [] arguments
  type_ <- symbol ":" *> expression
  pure (name_, args_, type_)

definition :: Parser Declaration
definition = do
  (name_, args_, type_) <- commonDeclaretion "def"
  body_ <- symbol "=>" *> expression
  pure (Definition name_ args_ type_ body_)

inductive :: Parser Declaration
inductive = do
  (name_, args_, type_) <- commonDeclaretion "inductive"
  guard (type_ == Top)
  cons_ <- symbol "=>" *> many constructor
  pure (Inductive name_ args_ cons_)

external :: Parser Declaration
external = do
  (name_, args_, type_) <- commonDeclaretion "external"
  body_ <- symbol "=>" *> stringLiteral
  pure (External name_ args_ type_ body_)

declare :: Parser Declaration
declare = do
  (name_, args_, type_) <- commonDeclaretion "declare"
  pure (Declare name_ args_ type_)

declaration :: Parser Declaration
declaration = definition <|> inductive <|> external <|> declare

module_ :: Parser [Declaration]
module_ = many declaration

parseModule :: Text -> Either Text [Declaration]
parseModule source =
  case runParser (unParser module_ <* eof) "file.ns" source of
    Right decls -> Right decls
    Left errors -> Left (pack (errorBundlePretty errors))
