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
  | FailToInferTypeOfWildCard Name
  | InvalidTypeFor Expr (Actual Type) (Expected Type)
  | UnsupportedPattern Expr
  | InvalidProductLength (Actual Int) (Expected Int)
  | ExpectedTypeFor Expr Type
  | TypeAnnotationIsNotMatchable
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
  let -- do not declare names, that are already declared as mutual
      newContextWithoutMutuals = filter (\(name, _) -> name `notElem` mutuals) newContext
      -- remove new names from mutual list
      mutualsWithoutNewContext = filter (`notElem` (fst <$> newContext)) mutuals
   in Env (reverse newContextWithoutMutuals <> context) mutualsWithoutNewContext callStack
  where
    sameName (name1, _) (name2, _) = name1 == name2

withMutualDeclarations :: [Name] -> TC a -> TC a
withMutualDeclarations newMutuals = modifyEnv \(Env context mutuals callStack) ->
  Env context (newMutuals <> mutuals) callStack

checkExprIs :: Type -> Expr -> TC ()
checkExprIs type_ expr = do
  actualType <- inferExpr expr
  unless (Value actualType == type_) do
    fail (InvalidTypeFor expr (Actual (Value actualType)) (Expected type_))

hasWildcards :: Expr -> Bool
hasWildcards (App _ args) = any hasWildcards args
hasWildcards (Annotated value type_) = hasWildcards value || hasWildcards type_
hasWildcards (Interpolation parts) = any hasWildcards parts
hasWildcards (Array elems) = any hasWildcards elems
hasWildcards (ArrayType t) = hasWildcards t
hasWildcards (Tuple elems) = any hasWildcards elems
hasWildcards (TupleType elems) = any hasWildcards elems
hasWildcards (Wildcard _) = True
hasWildcards (Match value cases) = hasWildcards value || any (\(pat, val) -> hasWildcards pat || hasWildcards val) cases
hasWildcards (Let bindings next) = any (\(LetBinding _ type_ value) -> hasWildcards type_ || hasWildcards value) bindings || hasWildcards next
hasWildcards _ = False

withPatternOf :: Type -> Expr -> TC a -> TC a
withPatternOf valueType pat next = do
  case (pat, valueType) of
    (Var {}, _) -> checkExprIs valueType pat >> next
    (App name args, _) -> do
      (argsTypes, constructorType) <- lookupForFunction name
      unless (valueType == Value constructorType) do
        fail (InvalidTypeFor pat (Actual (Value constructorType)) (Expected valueType))
      checkProduct (Value <$> argsTypes) args next
    (Annotated _ type_, _) | hasWildcards type_ -> fail TypeAnnotationIsNotMatchable
    (Annotated value type_, _) -> do
      unless (valueType == Value type_) do
        fail (ExpectedTypeFor pat valueType)
      withPatternOf (Value type_) value next
    (Number {}, _) -> checkExprIs valueType pat >> next
    (String {}, _) -> checkExprIs valueType pat >> next
    (Boolean {}, _) -> checkExprIs valueType pat >> next
    (Interpolation parts, _) -> traverseProduct (zip parts (repeat (Value StringType))) next
    (Array as, Value (ArrayType t)) -> traverseProduct (zip as (repeat (Value t))) next
    (Array {}, _) -> fail (ExpectedTypeFor pat valueType)
    (ArrayType a, _) -> withPatternOf (Value Top) a next
    (Tuple args, Value (TupleType argsTypes)) -> checkProduct (Value <$> argsTypes) args next
    (Tuple {}, _) -> fail (ExpectedTypeFor pat valueType)
    (TupleType as, _) -> traverseProduct (zip as (repeat (Value Top))) next
    (Wildcard name, _) -> withDeclared name valueType next
    (Match _ _, _) -> fail (UnsupportedPattern pat)
    (Let _ _, _) -> fail (UnsupportedPattern pat)
    (Top, _) -> checkExprIs valueType pat >> next
    (Bottom, _) -> checkExprIs valueType pat >> next
  where
    checkProduct formals pats next = do
      unless (length formals == length pats) do
        fail (InvalidProductLength (Actual (length pats)) (Expected (length formals)))
      traverseProduct (zip pats formals) next

    traverseProduct [] next = next
    traverseProduct ((pat, expectedPatType) : pats) next = do
      withPatternOf expectedPatType pat do
        traverseProduct pats next

withArguments :: Arguments -> TC a -> TC a
withArguments [] next = next
withArguments ((name_, type_) : args) next = do
  checkExprIs (Value Top) type_
  withDeclared name_ (Value type_) do
    withArguments args next

-- a, b - expression
-- x, f - reference
-- n    - number
-- s    - string
-- t    - type
inferExpr :: Expr -> TC Expr
--
--  x : t ∈ Г
--  ─────────
--  Г ⊢ x : t
inferExpr (Var x) = lookupForValue x
--
--  (t₁, …, tᵤ) → t ∈ Г; Г ⊢ a₁ : t₁, …, aᵤ : tᵤ
--  ────────────────────────────────────────────
--              Г ⊢ f(a₁, …, aᵤ) : t
inferExpr (App fun args) = do
  (formalArgsTypes, resultType) <- lookupForFunction fun
  actualArgsTypes <- for args inferExpr
  unless (formalArgsTypes == actualArgsTypes) do
    fail (InvalidArgumentTypeFor fun (Expected formalArgsTypes) (Actual actualArgsTypes))
  pure resultType
--
--   Г ⊢ a : t
--  ───────────
--  (a : t) : t
inferExpr (Annotated value type_) = do
  checkExprIs (Value type_) value
  pure type_
--
--  ──────────
--  n : number
inferExpr (Number _) = pure NumberType
--
--  ──────────
--  s : number
inferExpr (String _) = pure StringType
--
--  ───────────────────────────────
--  true : boolean, false : boolean
inferExpr (Boolean _) = pure BooleanType
--
--    Г ⊢ a₁ … aᵤ : string
--  ────────────────────────
--  Г ⊢ <a₁, …, aᵤ> : string
inferExpr (Interpolation parts) = do
  for parts (checkExprIs (Value StringType))
  pure StringType
--
--  ─────────────
--  [] : array(⊤)
inferExpr (Array []) = pure Top
--
--       Г ⊢ a₁ … aᵤ : t
--  ──────────────────────────
--  Г ⊢ [a₁, …, aᵤ] : array(t)
inferExpr (Array elems) = do
  elemTypes <- for elems inferExpr
  unless (all (== head elemTypes) elemTypes) do
    fail (InvalidArrayElementsTypes (Actual elemTypes))
  pure (ArrayType (head elemTypes))
--
--     Г ⊢ t : ⊤
--  ────────────────
--  Г ⊢ array(t) : ⊤
inferExpr (ArrayType elemType) = do
  elemTypeType <- inferExpr elemType
  checkExprIs (Value Top) elemTypeType
  pure Top
--
--       Г ⊢ a₁ : t₁, …, aᵤ : tᵤ
--  ──────────────────────────────────
--  Г ⊢ (a₁, …, aᵤ) : tuple(t₁, …, tᵤ)
inferExpr (Tuple elems) = TupleType <$> for elems inferExpr
--
--       Г ⊢ t₁ … tᵤ : ⊤
--  ─────────────────────────
--  Г ⊢ tuple(t₁, …, tᵤ) : ⊤
inferExpr (TupleType elemsTypes) = do
  for elemsTypes \elemType -> do
    checkExprIs (Value Top) elemType
  pure Top
inferExpr (Wildcard name) = fail (FailToInferTypeOfWildCard name)
--
--  ───────────────────────
--  Г ⊢ match a with {} : ⊥
inferExpr (Match a []) = pure Bottom
--
--   Г ⊢ a : t₁; ∀i, Г, wildcars(pᵢ) ⊢ pᵢ : t₁, bᵢ : t₂
--  ────────────────────────────────────────────────────
--     Г ⊢ match a with {p₁ => b₁, …, pᵤ => bᵤ} : t₂
inferExpr (Match a cases) = do
  withStackFrame ("match: " <> show a) do
    t1 <- inferExpr a
    valuesTypes <- for cases \(pat, value) -> do
      withStackFrame ("pattern: " <> show pat) do
        withPatternOf (Value t1) pat (inferExpr value)
    unless (all (== head valuesTypes) valuesTypes) do
      fail (InvalidArrayElementsTypes (Actual valuesTypes))
    pure (head valuesTypes)
--
--     Г ⊢ x : t
--  ────────────────
--  Г ⊢ let in x : t
inferExpr (Let [] next) = inferExpr next
--
--  Г ⊢ t₁ : ⊤, x : t₁;  Г, x : t₁ ⊢ let xs in b : t₂
--  ──────────────────────────────────────────────────
--           Г ⊢ let x : t₁ = a, xs in b : t₂
inferExpr (Let (LetBinding name type_ value : bindings) next) = do
  withStackFrame (unName name) do
    checkExprIs (Value Top) type_
    checkExprIs (Value type_) value
    withDeclared name (Value type_) do
      inferExpr (Let bindings next)
--
--  ──────
--  ⊤ : ⊤
inferExpr Top = pure Top
--
--  ──────
--  ⊥ : ⊤
inferExpr Bottom = pure Top

makeContextItem :: Name -> Arguments -> Expr -> (Name, Type)
makeContextItem name [] type_ = (name, Value type_)
makeContextItem name args type_ = (name, Function (snd <$> args) type_)

getContextItems :: Declaration -> [(Name, Type)]
getContextItems (Definition name args type_ _) = [makeContextItem name args type_]
getContextItems (Inductive name args constructors) = makeContextItem name args Top : constructorItems
  where
    constructorItems = do
      Constructor conName conArgs <- constructors
      pure (makeContextItem conName conArgs (Var name))
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
        checkExprIs (Value type_) body
  withDeclaredContextItem contextItem next
checkDeclaration decl@(Inductive name args constructors) next = do
  let contextItem : constructorItems = getContextItems decl
  checkUndeclared name
  withStackFrame (unName name) do
    withArguments args do
      withDeclaredContextItem contextItem do
        for_ constructors \(Constructor name args) -> do
          checkUndeclared name
          withArguments args (pure ())
  withDeclaredContextItems (contextItem : constructorItems) next
checkDeclaration decl@(External name args type_ _) next = do
  checkUndeclared name
  withStackFrame (unName name) do
    withArguments args (pure ())
  withDeclaredContextItems (getContextItems decl) next
checkDeclaration decl@(Declare name args type_) next = do
  checkUndeclared name
  withStackFrame (unName name) do
    withArguments args (pure ())
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
  [ ("number", Value Top),
    ("string", Value Top),
    ("boolean", Value Top),
    ("Record", Function [Top, Top] Top),
    ("the", Function [Top, Var "a"] (Var "a")),
    ("plus", Function [NumberType, NumberType] NumberType)
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
