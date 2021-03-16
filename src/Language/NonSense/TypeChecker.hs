{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Language.NonSense.TypeChecker where

import qualified Data.List as List
import qualified Data.Set as Set
import Language.NonSense.AST
import NSPrelude

newtype Expected a = Expected a
  deriving stock (Show)

newtype Actual a = Actual a
  deriving stock (Show)

data TypeCheckError
  = UnknownName Name
  | Unapplied Name
  | DuplicateDeclaration Name
  | FunctionUsedAsValue Name
  | ValueUsedAsFunction Name
  | TypeMismatch Expr (Expected Expr) (Actual Expr)
  | InvalidFunctionArgumentsCountFor Name (Expected Int) (Actual Int)
  | UnexpectedExpressionInPattern Expr
  | UnexpectedPatternInExpression Pattern
  | ArrayElementsShouldHaveTheSameType Expr
  | MatchCaseBranchesShouldHaveTheSameType (Actual [Type])
  --  | InvalidArgumentTypeFor Name (Expected [Expr]) (Actual [Expr])
  --  | InvalidArrayElementsTypes (Actual [Expr])
  --  | InvalidArgumentsLength Name (Actual Int) (Expected Int)
  --  | InvalidTypeFor Expr (Actual Expr) (Expected Expr)
  --  | InvalidProductLength (Actual Int) (Expected Int)
  --  | InvalidPatternTypeFor Pattern (Expected Expr)
  --  | UnexpectedPatternForTypeInference Pattern
  deriving stock (Show)

data FunctionType = FunctionType {arguments :: [Argument], result :: Type}
  deriving (Show)

type ContextType = Either FunctionType Type

data Environment = Environment
  { context :: [(Name, ContextType)],
    stack :: [(Name, Expr)],
    mutuals :: Set Name,
    traces :: [Text]
  }

newtype TC a = TC {runTC :: ReaderT Environment (Except (Environment, TypeCheckError)) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader Environment)
  deriving newtype (MonadError (Environment, TypeCheckError))

getContext :: TC [(Name, ContextType)]
getContext = TC (asks (.context))

getStack :: TC [(Name, Expr)]
getStack = TC (asks (.stack))

getMutuals :: TC (Set Name)
getMutuals = TC (asks (.mutuals))

throw :: TypeCheckError -> TC a
throw error = do
  environment <- ask
  throwError (environment, error)

trace :: Text -> TC a -> TC a
trace message = local \env -> env{traces = message : env.traces}

withArgumentInStack :: (Name, Expr) -> TC a -> TC a
withArgumentInStack (name, value) = local \env -> env {stack = (name, value) : env.stack}

checkUndeclared :: Name -> TC ()
checkUndeclared name = do
  trace ("Checking " <> name.value <> " is undeclared") do
    context <- getContext
    mutuals <- getMutuals
    case List.lookup name context of
      Just _ | name `notElem` mutuals -> throw (DuplicateDeclaration name)
      _ -> pure ()

lookupValue :: Name -> TC (Maybe Expr)
lookupValue name = do
  stack <- getStack
  pure (List.lookup name stack)

lookupType :: Name -> TC (ContextType)
lookupType name = do
  context <- getContext
  case List.lookup name context of
    Just type_ -> pure type_
    Nothing -> throw (UnknownName name)

lookupFunctionType :: Name -> TC FunctionType
lookupFunctionType name =
  lookupType name >>= \case
    Left type_ -> pure type_
    _ -> throw (ValueUsedAsFunction name)

lookupValueType :: Name -> TC Type
lookupValueType name =
  lookupType name >>= \case
    Right type_ -> pure type_
    _ -> throw (FunctionUsedAsValue name)

withDeclared :: (Name, ContextType) -> TC a -> TC a
withDeclared (name, type_) next =
  trace ("declared " <> name.value <> " as " <> show type_) do
    flip local next \env ->
      env{
          -- if name is in mutuals list, it is already declared
          context = if name `Set.member` env.mutuals then env.context else (name, type_) : env.context,
          mutuals = Set.delete name env.mutuals
         }

withDeclaredValue :: (Name, Type) -> TC a -> TC a
withDeclaredValue (name, type_) = withDeclared (name, Right type_)

withMutual :: (Name, ContextType) -> TC a -> TC a
withMutual (name, _) next =
  trace ("marked as mutual " <> name.value) do
    flip local next \env -> env{mutuals = Set.insert name env.mutuals}

many :: (a -> TC x -> TC x) -> [a] -> TC x -> TC x
many f xs next = foldr f next xs

checkIsSameFor :: Expr -> Expected Type -> Actual Type -> TC ()
checkIsSameFor expr expected@(Expected type1) actual@(Actual type2) = do
  type1' <- eval type1
  type2' <- eval type2
  unless (type1' == type2') do
    throw (TypeMismatch expr expected actual)

(!:) :: Expr -> Expr -> TC ()
expr !: expectedType = do
  actualType <- infer expr
  checkIsSameFor expr (Expected expectedType) (Actual actualType)

eval :: Expr -> TC Expr
eval (Var name) = lookupValue name >>= \case Just value -> pure value; Nothing -> pure (Var name)
eval (App "the" [a, b]) = eval b
eval (App "plus" [a, b]) = do
  traverse eval [a, b] >>= \case
    [Number a', Number b'] -> pure (Number (a' + b'))
    arguments' -> pure (App "plus" arguments')
-- TODO: eval all function
eval (App name arguments) = App name <$> traverse eval arguments
eval (Annotated value type_) = eval value
eval (Number number) = pure (Number number)
eval (String string) = pure (String string)
eval (Boolean boolean) = pure (Boolean boolean)
eval (Interpolation parts) = do
  parts' <- traverse eval parts
  case isPureInterpolation parts' of
    Just parts'' -> pure (String (mconcat parts''))
    _ -> pure (Interpolation parts')
  where
    isPureInterpolation :: [Expr] -> Maybe [Text]
    isPureInterpolation [] = pure []
    isPureInterpolation (String s : parts) = (s :) <$> isPureInterpolation parts
    isPureInterpolation _ = Nothing
eval (Array elements) = Array <$> traverse eval elements
eval (Tuple elements) = Tuple <$> traverse eval elements
-- TODO: eval match
eval (Match value caseBranchs) = Match <$> eval value <*> traverse evalCaseBranch caseBranchs
  where
    evalCaseBranch CaseBranch {pattern_, value} = CaseBranch <$> eval pattern_ <*> eval value
-- TODO: eval match
eval (Let bindings next) = Let <$> traverse evalLetBinding bindings <*> eval next
  where
    evalLetBinding LetBinding {pattern_, value} = LetBinding <$> eval pattern_ <*> eval value
eval Top = pure Top
eval Bottom = pure Bottom
eval NumberType = pure NumberType
eval StringType = pure StringType
eval BooleanType = pure BooleanType
eval (ArrayType elementType) = ArrayType <$> eval elementType
eval (TupleType elementTypes) = TupleType <$> traverse eval elementTypes
eval WildcardPattern = pure WildcardPattern
eval (DotPattern name) = eval (Var name)
eval (InferPattern name) = pure (InferPattern name)
eval Meta = pure Meta

canBeApplied :: Expr -> Bool
canBeApplied Var {} = True
canBeApplied (App name arguments) = all canBeApplied arguments
canBeApplied (Annotated value _) = canBeApplied value
canBeApplied Number {} = True
canBeApplied String {} = True
canBeApplied Boolean {} = True
canBeApplied (Interpolation parts) = all canBeApplied parts
canBeApplied (Array elements) = all canBeApplied elements
canBeApplied (Tuple elements) = all canBeApplied elements
canBeApplied Match {} = False
canBeApplied Let {} = False
canBeApplied Top = True
canBeApplied Bottom = True
canBeApplied NumberType = True
canBeApplied StringType = True
canBeApplied BooleanType = True
canBeApplied (ArrayType elementType) = canBeApplied elementType
canBeApplied (TupleType elementTypes) = all canBeApplied elementTypes
canBeApplied WildcardPattern = False
canBeApplied DotPattern {} = True
canBeApplied InferPattern {} = False
canBeApplied Meta = False

withAppliedArgumentsInPattern :: [(Argument, Expr)] -> TC a -> TC a
withAppliedArgumentsInPattern [] next = next
withAppliedArgumentsInPattern ((argument, value) : arguments) next | canBeApplied value = do
  trace ("unify argument " <> argument.name.value <> " as value " <> show value) do
    value !: argument.type_
    value' <- eval value
    withArgumentInStack (argument.name, value') do
      withAppliedArgumentsInPattern arguments next
withAppliedArgumentsInPattern ((argument, value) : arguments) next = do
  evaledArgumentType <- eval argument.type_
  withPatternOfType evaledArgumentType value do
    withAppliedArgumentsInPattern arguments next

withPatternOfType :: Type -> Expr -> TC a -> TC a
withPatternOfType type_ pattern_ next =
  trace ("move pattern to context " <> show pattern_) do
    case pattern_ of
      Var name -> throw (UnexpectedExpressionInPattern pattern_)
      App name arguments -> do
        signature <- lookupFunctionType name
        unless (length signature.arguments == length arguments) do
          throw (InvalidFunctionArgumentsCountFor name (Expected (length arguments)) (Actual (length signature.arguments)))
        withAppliedArgumentsInPattern (zip signature.arguments arguments) do
          checkIsSameFor pattern_ (Expected type_) (Actual signature.result)
          next
      Annotated value valueType -> do
        valueType !: Top
        checkIsSameFor value (Expected type_) (Actual valueType)
        withPatternOfType type_ value next
      Number _ -> checkIsSameFor pattern_ (Expected type_) (Actual NumberType) >> next
      String _ -> checkIsSameFor pattern_ (Expected type_) (Actual StringType) >> next
      Boolean _ -> checkIsSameFor pattern_ (Expected type_) (Actual BooleanType) >> next
      Interpolation parts -> do
        checkIsSameFor pattern_ (Expected type_) (Actual StringType)
        many (withPatternOfType StringType) parts next
      -- TODO: array pattern
      Array [] -> checkIsSameFor pattern_ (Expected type_) (Actual (ArrayType Meta)) >> next
      Array elements -> do
        elementsTypes <- traverse infer elements
        let elementType = head elementsTypes
        unless (all (== elementType) elementsTypes) do
          throw (ArrayElementsShouldHaveTheSameType (Array elements))
        checkIsSameFor pattern_ (Expected type_) (Actual (ArrayType elementType))
        many (withPatternOfType elementType) elements next
      -- TODO: tuple pattern
      Tuple elements -> do
        elementsTypes <- traverse infer elements

        trace (show (elementsTypes)) $ checkIsSameFor pattern_ (Expected type_) (Actual (TupleType elementsTypes))
        let go [] = next
            go ((e, t) : es) = withPatternOfType t e (go es)
        go (zip elements elementsTypes)
      Match {} -> throw (UnexpectedExpressionInPattern pattern_)
      Let {} -> throw (UnexpectedExpressionInPattern pattern_)
      Top -> checkIsSameFor pattern_ (Expected type_) (Actual Top) >> next
      Bottom -> checkIsSameFor pattern_ (Expected type_) (Actual Top) >> next
      NumberType -> checkIsSameFor pattern_ (Expected type_) (Actual Top) >> next
      StringType -> checkIsSameFor pattern_ (Expected type_) (Actual Top) >> next
      BooleanType -> checkIsSameFor pattern_ (Expected type_) (Actual Top) >> next
      ArrayType elementType -> do
        checkIsSameFor pattern_ (Expected type_) (Actual Top) >> next
        withPatternOfType Top elementType next
      TupleType elementTypes -> do
        checkIsSameFor pattern_ (Expected type_) (Actual Top) >> next
        many (withPatternOfType Top) elementTypes next
      WildcardPattern -> next
      DotPattern name -> do
        nameType <- lookupValueType name
        checkIsSameFor pattern_ (Expected type_) (Actual nameType)
        next
      InferPattern name -> withDeclaredValue (name, type_) next
      Meta -> next

withAppliedArguments :: [(Argument, Expr)] -> TC a -> TC a
withAppliedArguments [] next = next
withAppliedArguments ((argument, value) : arguments) next = do
  trace ("apply argument " <> argument.name.value <> " to " <> show value) do
    value !: argument.type_
    value' <- eval value
    withArgumentInStack (argument.name, value') do
      withAppliedArguments arguments next

-- a, b - expression
-- x, f - reference
-- n    - number
-- s    - string
-- t    - type
-- p    - pattern
infer :: Expr -> TC Type
--  x : t ∈ Г
--  ─────────
--  Г ⊢ x : t
infer (Var x) = lookupValueType x
--
--  (t₁, …, tᵤ) → t ∈ Г; Г ⊢ a₁ : t₁, …, aᵤ : tᵤ
--  ────────────────────────────────────────────
--              Г ⊢ f(a₁, …, aᵤ) : t
infer (App name arguments) = do
  trace ("infer application type " <> name.value) do
    signature <- lookupFunctionType name
    unless (length signature.arguments == length arguments) do
      throw (InvalidFunctionArgumentsCountFor name (Expected (length signature.arguments)) (Actual (length arguments)))
    withAppliedArguments (zip signature.arguments arguments) do
      eval signature.result
--
--   Г ⊢ a : t
--  ───────────
--  (a : t) : t
infer (Annotated value type_) = do
  value !: type_
  pure type_
--
--  ──────────
--  n : number
infer (Number _) = pure NumberType
--
--  ──────────
--  s : string
infer (String _) = pure StringType
--
--  ───────────────────────────────
--  true : boolean, false : boolean
infer (Boolean _) = pure BooleanType
--
--    Г ⊢ a₁ … aᵤ : string
--  ────────────────────────
--  Г ⊢ <a₁, …, aᵤ> : string
infer (Interpolation parts) = do
  traverse (!: StringType) parts
  pure StringType
--
--  ─────────────
--  [] : array(_)
infer (Array []) = pure (ArrayType Meta)
--
--       Г ⊢ a₁ … aᵤ : t
--  ──────────────────────────
--  Г ⊢ [a₁, …, aᵤ] : array(t)
infer (Array elements) = do
  elementsTypes <- traverse infer elements
  unless (all (== head elementsTypes) elementsTypes) do
    throw (ArrayElementsShouldHaveTheSameType (Array elements))
  pure (ArrayType (head elementsTypes))
--
--       Г ⊢ a₁ : t₁, …, aᵤ : tᵤ
--  ──────────────────────────────────
--  Г ⊢ (a₁, …, aᵤ) : tuple(t₁, …, tᵤ)
infer (Tuple elements) = TupleType <$> traverse infer elements
--
--  ───────────────────────
--  Г ⊢ match a with {} : ⊥
infer (Match a []) = pure Bottom
--
--   Г ⊢ a : t₁; ∀i, Г, bindings(pᵢ) ⊢ pᵢ : t₁, bᵢ : t₂
--  ────────────────────────────────────────────────────
--     Г ⊢ match a with {p₁ := b₁, …, pᵤ := bᵤ} : t₂
infer (Match value cases) = do
  trace ("match " <> show value) do
    valueType <- infer value
    casesTypes <- for cases \caseBranch -> do
      withPatternOfType valueType caseBranch.pattern_ do
        trace ("match-rhs " <> show caseBranch.value) do
          eval =<< infer caseBranch.value
    unless (all (== head casesTypes) casesTypes) do
      throw (MatchCaseBranchesShouldHaveTheSameType (Actual casesTypes))
    pure (head casesTypes)
--
--     Г ⊢ x : t
--  ────────────────
--  Г ⊢ let in x : t
infer (Let [] next) = infer next
--
--  Г ⊢ a : t₁;  Г, bindings(p) ⊢ p : t₁, let xs in b : t₂
--  ───────────────────────────────────────────────────────
--               Г ⊢ let p := a, xs in b : t₂
infer (Let (binding : bindings) next) = do
  trace ("let-binding " <> show binding.pattern_) do
    type_ <- infer binding.value
    withPatternOfType type_ binding.pattern_ do
      infer (Let bindings next)
--
--  ──────
--  ⊤ : ⊤
infer Top = pure Top
--
--  ──────
--  ⊥ : ⊤
infer Bottom = pure Top
--
--  ──────────
--  number : ⊤
infer NumberType = pure Top
--
--  ──────────
--  string : ⊤
infer StringType = pure Top
--
--  ───────────
--  boolean : ⊤
infer BooleanType = pure Top
--
--     Г ⊢ t : ⊤
--  ────────────────
--  Г ⊢ array(t) : ⊤
infer (ArrayType elementType) = do
  elementType !: Top
  pure Top
--
--       Г ⊢ t₁ … tᵤ : ⊤
--  ─────────────────────────
--  Г ⊢ tuple(t₁, …, tᵤ) : ⊤
infer (TupleType elementsTypes) = do
  traverse (!: Top) elementsTypes
  pure Top
infer WildcardPattern = pure Meta
infer (DotPattern name) = lookupValueType name
infer (InferPattern name) = pure Meta
infer Meta = pure Top

makeContextElement :: Name -> [Argument] -> Expr -> (Name, ContextType)
makeContextElement name [] type_ = (name, Right type_)
makeContextElement name arguments result = (name, Left FunctionType {arguments, result})

makeContextElements :: Declaration -> [(Name, ContextType)]
makeContextElements (Definition name arguments type_ _) = [makeContextElement name arguments type_]
makeContextElements (Enum typeName constructors typeArguments) = typeItem : constructorItems
  where
    typeItem = makeContextElement typeName typeArguments Top
    constructorItems = fmap makeConstructorItem constructors
    makeConstructorItem constructor =
      makeContextElement constructor.name (typeArguments <> constructor.arguments) constructorType
    constructorType = case typeArguments of
      [] -> Var typeName
      _ -> App typeName [Var a.name | a <- typeArguments]
makeContextElements (External name arguments type_ _) = [makeContextElement name arguments type_]
makeContextElements (Declare name arguments type_) = [makeContextElement name arguments type_]
-- ignore nested declarations
makeContextElements (Mutual declarations) = []

withArguments :: [Argument] -> TC a -> TC a
withArguments [] next = next
withArguments (argument : arguments) next = do
  trace ("def-arg: " <> argument.name.value <> " = " <> show argument.type_) do
    argument.type_ !: Top
    withDeclaredValue (argument.name, argument.type_) do
      withArguments arguments next

checkDeclaration :: Declaration -> TC a -> TC a
checkDeclaration decl@(Definition name arguments type_ body) next = do
  let [contextElement] = makeContextElements decl
  checkUndeclared name
  trace ("def: " <> name.value) do
    withDeclared contextElement do
      withArguments arguments do
        type_ !: Top
        body !: type_
  withDeclared contextElement next
checkDeclaration decl@(Enum name constructors arguments) next = do
  let contextElement : constructorItems = makeContextElements decl
  checkUndeclared name
  trace ("enum: " <> name.value) do
    withDeclared contextElement do
      withArguments arguments do
        for_ constructors \constructor -> do
          checkUndeclared constructor.name
          withArguments constructor.arguments (pure ())
  many withDeclared (contextElement : constructorItems) next
checkDeclaration decl@(External name arguments type_ _) next = do
  checkUndeclared name
  trace ("external: " <> name.value) do
    withArguments arguments do
      type_ !: Top
  many withDeclared (makeContextElements decl) next
checkDeclaration decl@(Declare name args type_) next = do
  checkUndeclared name
  trace ("declare: " <> name.value) do
    withArguments args do
      type_ !: Top
  many withDeclared (makeContextElements decl) next
checkDeclaration decl@(Mutual declarations) next = do
  let contextItems = foldMap makeContextElements declarations
  for contextItems \(name, _) -> do
    checkUndeclared name
  many withDeclared contextItems do
    many withMutual contextItems do
      many checkDeclaration declarations next

defaultContext :: [(Name, ContextType)]
defaultContext =
  [ ("the", Left FunctionType {arguments = [Argument "a" Top, Argument "b" (Var "a")], result = Var "a"}),
    ("plus", Left FunctionType {arguments = [Argument "a" NumberType, Argument "b" NumberType], result = NumberType})
  ]

defaultEnvironment :: Environment
defaultEnvironment =
  Environment
    { context = defaultContext,
      stack = [],
      mutuals = Set.empty,
      traces = []
    }

check :: [Declaration] -> Maybe (Environment, TypeCheckError)
check decls =
  case runExcept (runReaderT (runTC (many checkDeclaration decls (pure ()))) defaultEnvironment) of
    Right () -> Nothing
    Left err -> Just err

printEnv :: Environment -> Text
printEnv Environment {context, stack, mutuals, traces} =
  mconcat
    [ unlines ("\nTrace: " : fmap (\x -> "- " <> x) traces),
      "\nMutuals: " <> show mutuals,
      "\nStack: " <> show stack,
      "\nContext: " <> show context
    ]

printError :: TypeCheckError -> Text
printError err = "TypeCheckError: " <> show err
