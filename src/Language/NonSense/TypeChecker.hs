{-# LANGUAGE TypeApplications #-}

module Language.NonSense.TypeChecker where

import Language.NonSense.AST hiding (Type)
import NSPrelude
import qualified Prelude (lookup)

newtype Expected a = Expected a
  deriving stock (Show)

newtype Actual a = Actual a
  deriving stock (Show)

data TypeCheckError
  = UnknownVariable Name
  | InvalidArgumentTypeFor Name (Expected [Expr]) (Actual [Expr])
  | AlreadyDeclared Name
  | FunctionUsedAsValue Name
  | ValueUsedAsFunction Name
  | InvalidArrayElementsTypes (Actual [Expr])
  | InvalidTypeFor Expr (Actual Type) (Expected Type)
  | InvalidProductLength (Actual Int) (Expected Int)
  | InvalidPatternTypeFor Pattern (Expected Expr)
  | UnexpectedPatternForTypeInference Pattern
  deriving stock (Show)

data Type
  = Function [Expr] Expr
  | Value Expr
  deriving stock (Eq, Show)

type Context = [(Name, Type)]

type CallStack = [Text]

data Env = Env
  { envContext :: Context,
    envMutuals :: [Name],
    envCallStack :: CallStack
  }

newtype TC a = TC {runTC :: ReaderT Env (Either (Env, TypeCheckError)) a}
  deriving newtype (Functor, Applicative, Monad)

getContext :: TC Context
getContext = TC (asks envContext)

getMutuals :: TC [Name]
getMutuals = TC (asks envMutuals)

getCallStack :: TC CallStack
getCallStack = TC (asks envCallStack)

modifyEnv :: (Env -> Env) -> TC a -> TC a
modifyEnv f (TC next) = TC (local f next)

fail :: TypeCheckError -> TC a
fail error = do
  env <- TC ask
  TC (lift (Left (env, error)))

lookup :: Name -> TC Type
lookup name = do
  context <- getContext
  case Prelude.lookup name context of
    Just type_ -> pure type_
    Nothing -> fail (UnknownVariable name)

checkUndeclared :: Name -> TC ()
checkUndeclared name = do
  context <- getContext
  mutuals <- getMutuals
  case Prelude.lookup name context of
    Just _ | name `notElem` mutuals -> fail (AlreadyDeclared name)
    _ -> pure ()

lookupForFunction :: Name -> TC ([Expr], Expr)
lookupForFunction name =
  lookup name >>= \case
    Function args result -> pure (args, result)
    _ -> fail (ValueUsedAsFunction name)

lookupForValue :: Name -> TC Expr
lookupForValue name =
  lookup name >>= \case
    Value type_ -> pure type_
    _ -> fail (FunctionUsedAsValue name)

withStackFrame :: Text -> TC a -> TC a
withStackFrame name = modifyEnv \(Env context mutuals callStack) ->
  Env context mutuals (name : callStack)

withDeclared :: Name -> Type -> TC a -> TC a
withDeclared name type_ = withDeclaredContextItem (name, type_)

withDeclaredContextItem :: (Name, Type) -> TC a -> TC a
withDeclaredContextItem item = withDeclaredContextItems [item]

withDeclaredContextItems :: [(Name, Type)] -> TC a -> TC a
withDeclaredContextItems newContext = modifyEnv \(Env context mutuals callStack) ->
  let -- do not declare names, that are already declared as mutuals
      newContextWithoutMutuals = filter (\(name, _) -> name `notElem` mutuals) newContext
      -- remove new names from the mutuals list
      mutualsWithoutNewContext = filter (`notElem` (fst <$> newContext)) mutuals
   in Env (reverse newContextWithoutMutuals <> context) mutualsWithoutNewContext callStack
  where
    sameName (name1, _) (name2, _) = name1 == name2

withMutualDeclarations :: [Name] -> TC a -> TC a
withMutualDeclarations newMutuals = modifyEnv \(Env context mutuals callStack) ->
  Env context (newMutuals <> mutuals) callStack

exprHasType :: Expr -> Expr -> TC ()
exprHasType expr type_ = do
  actualType <- inferExprType expr
  unless (actualType == type_) do
    fail (InvalidTypeFor expr (Actual (Value actualType)) (Expected (Value type_)))

patternHasType :: Pattern -> Expr -> TC ()
patternHasType pat type_ = do
  actualType <- inferPatternType pat
  unless (actualType == type_) do
    fail (InvalidPatternTypeFor pat (Expected type_))

inferPatternType :: Pattern -> TC Expr
inferPatternType PatternWildcard = fail (UnexpectedPatternForTypeInference PatternWildcard)
inferPatternType (PatternAnnotated pat type_) = do
  type_ `exprHasType` ExprTop
  pure type_
inferPatternType (PatternVar name) = fail (UnexpectedPatternForTypeInference (PatternVar name))
inferPatternType (PatternDot name) = inferExprType (ExprVar name)
inferPatternType (PatternConstructor name arguments) = do
  (_, resultType) <- lookupForFunction name
  pure resultType
inferPatternType (PatternNumber _) = pure ExprNumberType
inferPatternType (PatternString _) = pure ExprStringType
inferPatternType (PatternBoolean _) = pure ExprBooleanType
inferPatternType (PatternInterpolation _) = pure ExprStringType
inferPatternType (PatternArray elements) = fail (UnexpectedPatternForTypeInference (PatternArray elements))
inferPatternType (PatternTuple elements) = fail (UnexpectedPatternForTypeInference (PatternTuple elements))

-- TODO: merge patternHasType and withPatternOfType
withPatternOfType :: Expr -> Pattern -> TC a -> TC a
withPatternOfType valueType pat next = do
  case pat of
    PatternWildcard -> next
    PatternAnnotated value type_ -> do
      type_ `exprHasType` ExprTop
      unless (valueType == type_) do
        fail (InvalidPatternTypeFor pat (Expected valueType))
      withPatternOfType type_ value next
    PatternVar name -> withDeclared name (Value valueType) next
    PatternDot name -> ExprVar name `exprHasType` valueType >> next
    PatternConstructor name args -> do
      (argsTypes, constructorType) <- lookupForFunction name
      unless (valueType == constructorType) do
        fail (InvalidPatternTypeFor pat (Expected valueType))
      checkProductTypes argsTypes args next
    PatternNumber _ -> pat `patternHasType` valueType >> next
    PatternString {} -> pat `patternHasType` valueType >> next
    PatternBoolean {} -> pat `patternHasType` valueType >> next
    PatternInterpolation parts -> traverseProduct (zip parts (repeat ExprStringType)) next
    PatternArray elements ->
      case valueType of
        ExprArrayType elementType ->
          traverseProduct (zip elements (repeat elementType)) next
        _ -> fail (InvalidPatternTypeFor pat (Expected valueType))
    PatternTuple elements ->
      case valueType of
        ExprTupleType elementsTypes ->
          checkProductTypes elementsTypes elements next
        _ -> fail (InvalidPatternTypeFor pat (Expected valueType))
  where
    checkProductTypes types pats next = do
      unless (length types == length pats) do
        fail (InvalidProductLength (Actual (length pats)) (Expected (length types)))
      traverseProduct (zip pats types) next

    traverseProduct [] next = next
    traverseProduct ((pat, expectedPatType) : pats) next = do
      withPatternOfType expectedPatType pat do
        traverseProduct pats next

withArguments :: [Argument] -> TC a -> TC a
withArguments [] next = next
withArguments (Argument name_ type_ : args) next = do
  type_ `exprHasType` ExprTop
  withDeclared name_ (Value type_) do
    withArguments args next

-- a, b - expression
-- x, f - reference
-- n    - number
-- s    - string
-- t    - type
-- p    - pattern
inferExprType :: Expr -> TC Expr
--
--  x : t ∈ Г
--  ─────────
--  Г ⊢ x : t
inferExprType (ExprVar x) = lookupForValue x
--
--  (t₁, …, tᵤ) → t ∈ Г; Г ⊢ a₁ : t₁, …, aᵤ : tᵤ
--  ────────────────────────────────────────────
--              Г ⊢ f(a₁, …, aᵤ) : t
inferExprType (ExprApp name args) = do
  (formalArgsTypes, resultType) <- lookupForFunction name
  actualArgsTypes <- traverse inferExprType args
  unless (formalArgsTypes == actualArgsTypes) do
    fail (InvalidArgumentTypeFor name (Expected formalArgsTypes) (Actual actualArgsTypes))
  pure resultType
--
--   Г ⊢ a : t
--  ───────────
--  (a : t) : t
inferExprType (ExprAnnotated value type_) = do
  value `exprHasType` type_
  pure type_
--
--  ──────────
--  n : number
inferExprType (ExprNumber _) = pure ExprNumberType
--
--  ──────────
--  s : string
inferExprType (ExprString _) = pure ExprStringType
--
--  ───────────────────────────────
--  true : boolean, false : boolean
inferExprType (ExprBoolean _) = pure ExprBooleanType
--
--    Г ⊢ a₁ … aᵤ : string
--  ────────────────────────
--  Г ⊢ <a₁, …, aᵤ> : string
inferExprType (ExprInterpolation parts) = do
  traverse (`exprHasType` ExprStringType) parts
  pure ExprStringType
--
--  ─────────────
--  [] : array(⊤)
inferExprType (ExprArray []) = pure ExprTop
--
--       Г ⊢ a₁ … aᵤ : t
--  ──────────────────────────
--  Г ⊢ [a₁, …, aᵤ] : array(t)
inferExprType (ExprArray elems) = do
  elemTypes <- traverse inferExprType elems
  unless (all (== head elemTypes) elemTypes) do
    fail (InvalidArrayElementsTypes (Actual elemTypes))
  pure (ExprArrayType (head elemTypes))
--
--       Г ⊢ a₁ : t₁, …, aᵤ : tᵤ
--  ──────────────────────────────────
--  Г ⊢ (a₁, …, aᵤ) : tuple(t₁, …, tᵤ)
inferExprType (ExprTuple elems) = ExprTupleType <$> traverse inferExprType elems
--
--  ───────────────────────
--  Г ⊢ match a with {} : ⊥
inferExprType (ExprMatch a []) = pure ExprBottom
--
--   Г ⊢ a : t₁; ∀i, Г, bindings(pᵢ) ⊢ pᵢ : t₁, bᵢ : t₂
--  ────────────────────────────────────────────────────
--     Г ⊢ match a with {p₁ := b₁, …, pᵤ := bᵤ} : t₂
inferExprType (ExprMatch a cases) = do
  withStackFrame ("match: " <> show a) do
    t1 <- inferExprType a
    valuesTypes <- for cases \(CaseBranch pat value) -> do
      withStackFrame ("pattern: " <> show pat) do
        withPatternOfType t1 pat (inferExprType value)
    unless (all (== head valuesTypes) valuesTypes) do
      fail (InvalidArrayElementsTypes (Actual valuesTypes))
    pure (head valuesTypes)
--
--     Г ⊢ x : t
--  ────────────────
--  Г ⊢ let in x : t
inferExprType (ExprLet [] next) = inferExprType next
--
--  Г ⊢ a : t₁;  Г, bindings(p) ⊢ p : t₁, let xs in b : t₂
--  ───────────────────────────────────────────────────────
--               Г ⊢ let p := a, xs in b : t₂
inferExprType (ExprLet (LetBinding pat value : bindings) next) = do
  withStackFrame (show pat) do
    type_ <- inferExprType value
    withPatternOfType type_ pat do
      inferExprType (ExprLet bindings next)
--
--  ──────────
--  number : ⊤
inferExprType ExprNumberType = pure ExprTop
--
--  ──────────
--  string : ⊤
inferExprType ExprStringType = pure ExprTop
--
--  ───────────
--  boolean : ⊤
inferExprType ExprBooleanType = pure ExprTop
--
--     Г ⊢ t : ⊤
--  ────────────────
--  Г ⊢ array(t) : ⊤
inferExprType (ExprArrayType elemType) = do
  elemType `exprHasType` ExprTop
  pure ExprTop
--
--       Г ⊢ t₁ … tᵤ : ⊤
--  ─────────────────────────
--  Г ⊢ tuple(t₁, …, tᵤ) : ⊤
inferExprType (ExprTupleType elemsTypes) = do
  traverse (`exprHasType` ExprTop) elemsTypes
  pure ExprTop
--
--  ──────
--  ⊤ : ⊤
inferExprType ExprTop = pure ExprTop
--
--  ──────
--  ⊥ : ⊤
inferExprType ExprBottom = pure ExprTop

makeContextItem :: Name -> [Argument] -> Expr -> (Name, Type)
makeContextItem name [] type_ = (name, Value type_)
makeContextItem name args type_ = (name, Function [type_ | Argument name type_ <- args] type_)

getContextItems :: Declaration -> [(Name, Type)]
getContextItems (Definition name args type_ _) = [makeContextItem name args type_]
getContextItems (Enum name constructors) = makeContextItem name [] ExprTop : constructorItems
  where
    constructorItems = [makeContextItem conName conArgs (ExprVar name) | Constructor conName conArgs <- constructors]
getContextItems (External name args type_ _) = [makeContextItem name args type_]
getContextItems (Declare name args type_) = [makeContextItem name args type_]
-- ignore nested declarations
getContextItems (Mutual declarations) = []

checkDeclaration :: Declaration -> TC a -> TC a
checkDeclaration decl@(Definition name args type_ body) next = do
  let [contextItem] = getContextItems decl
  checkUndeclared name
  withStackFrame (unName name) do
    withArguments args do
      withDeclaredContextItem contextItem do
        type_ `exprHasType` ExprTop
        body `exprHasType` type_
  withDeclaredContextItem contextItem next
checkDeclaration decl@(Enum name constructors) next = do
  let contextItem : constructorItems = getContextItems decl
  checkUndeclared name
  withStackFrame (unName name) do
    withDeclaredContextItem contextItem do
      for_ constructors \(Constructor name args) -> do
        checkUndeclared name
        withArguments args (pure ())
  withDeclaredContextItems (contextItem : constructorItems) next
checkDeclaration decl@(External name args type_ _) next = do
  checkUndeclared name
  withStackFrame (unName name) do
    withArguments args do
      type_ `exprHasType` ExprTop
  withDeclaredContextItems (getContextItems decl) next
checkDeclaration decl@(Declare name args type_) next = do
  checkUndeclared name
  withStackFrame (unName name) do
    withArguments args do
      type_ `exprHasType` ExprTop
  withDeclaredContextItems (getContextItems decl) next
checkDeclaration decl@(Mutual declarations) next = do
  let contextItems = foldMap getContextItems declarations
  for contextItems \(name, _) -> do
    checkUndeclared name
  withDeclaredContextItems contextItems do
    withMutualDeclarations (fst <$> contextItems) do
      checkDeclarations declarations (pure ())
    next

checkDeclarations :: [Declaration] -> TC a -> TC a
checkDeclarations [] next = next
checkDeclarations (decl : decls) next = do
  checkDeclaration decl do
    checkDeclarations decls next

defaultContext :: Context
defaultContext =
  [ ("the", Function [ExprTop, ExprVar "a"] (ExprVar "a")),
    ("plus", Function [ExprNumberType, ExprNumberType] ExprNumberType)
  ]

check :: [Declaration] -> Maybe (Env, TypeCheckError)
check decls =
  case runReaderT (runTC (checkDeclarations decls (pure ()))) (Env defaultContext [] []) of
    Right () -> Nothing
    Left err -> Just err

printCallStack :: CallStack -> Text
printCallStack stack = unlines ("\nCallstack: " : fmap (\x -> "- " <> show x) stack)

printContext :: Context -> Text
printContext context = "\nContext: " <> show context

printMutuals :: [Name] -> Text
printMutuals mutuals = "\nMutuals: " <> show mutuals

printEnv :: Env -> Text
printEnv (Env context mutuals callStack) =
  mconcat
    [ printCallStack callStack,
      printMutuals mutuals,
      printContext context
    ]

printError :: TypeCheckError -> Text
printError err = "TypeCheckError: " <> show err
