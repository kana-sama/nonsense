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
keywords = ["in", "let", "def", "inductive", "external", "type", "match", "extends", "keyof", "typeof"]

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
keyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

name :: Parser Name
name = try do
  n <- lexeme $ Name . pack <$> liftA2 (:) letterChar (many (alphaNumChar <|> oneOf ['_', '-', '?']))
  when (unName n `elem` keywords) do
    failure (Just (Label (NonEmpty.fromList "name can't be keyword"))) mempty
  pure n

comma :: Parser ()
comma = void (symbol ",")

array :: Parser Expr
array = Array <$> between (symbol "[") (symbol "]") (expression `sepBy` comma)

object :: Parser Expr
object = Object <$> between (symbol "{") (symbol "}") (keyValue `sepBy` comma)
  where
    keyValue = do
      key <- stringLiteral
      symbol ":"
      value <- expression
      pure (key, value)

app :: Parser Expr
app = do
  function <- name
  arguments <- between (symbol "(") (symbol ")") (expression `sepBy` comma)
  pure (App function arguments)

universum :: Parser Expr
universum = U <$ symbol "U"

match :: Parser Expr
match = do
  keyword "match"
  expr <- expression
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
      object,
      wildrcard,
      match,
      let_,
      try universum,
      try app,
      Var <$> name
    ]

arguments :: Parser Arguments
arguments = between (symbol "(") (symbol ")") (argument `sepBy` comma)
  where
    argument = do
      argName <- name
      symbol ":"
      argType <- expression
      pure (argName, argType)

definition :: Parser Declaration
definition = do
  keyword "def"
  defName <- name
  defArgs <- option [] arguments
  defType <- symbol ":" *> expression
  defBody <- symbol "=>" *> expression
  pure (Definition defName defArgs defType defBody)

constructor :: Parser Constructor
constructor = do
  symbol "|"
  conName <- name
  conArgs <- option [] arguments
  pure (Constructor conName conArgs)

inductive :: Parser Declaration
inductive = do
  keyword "inductive"
  indName <- name
  indArgs <- option [] arguments
  indCons <- symbol "=>" *> many constructor
  pure (Inductive indName indArgs indCons)

external :: Parser Declaration
external = do
  keyword "external"
  extName <- name
  extArgs <- option [] arguments
  extType <- optional (symbol ":" *> expression)
  extBody <- symbol "=>" *> stringLiteral
  pure (External extName extArgs extType extBody)

declaration :: Parser Declaration
declaration = definition <|> inductive <|> external

module_ :: Parser [Declaration]
module_ = many declaration

parseModule :: Text -> Either Text [Declaration]
parseModule source =
  case runParser (unParser module_ <* eof) "file.ns" source of
    Right decls -> Right decls
    Left errors -> Left (pack (errorBundlePretty errors))
