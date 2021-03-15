{-# LANGUAGE TypeApplications #-}

module Language.NonSense.TypeChecker where

import qualified Data.Text as Text (init)
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
  --  | InvalidPatternsType (Expected Expr) (Actual Expr)
  --  | InvalidBranchesTypes (Actual [Expr])
  --  | TypeIsNotTypeFor Name (Actual Expr)
  --  | InvalidArgumentsCounts Name (Expected Int) (Actual Int)
  deriving stock (Show)

data Type
  = Function [Expr] Expr
  | Value Expr
  deriving stock (Eq, Show)

type Context = [(Name, Type)]

type CallStack = [Text]

data Env = Env {envContext :: Context, envCallStack :: CallStack}

newtype TC a = TC {runTC :: ReaderT Env (Either (Env, TypeCheckError)) a}
  deriving newtype (Functor, Applicative, Monad)

getContext :: TC Context
getContext = TC do
  asks envContext

getCallStack :: TC CallStack
getCallStack = TC do
  asks envCallStack

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
  case Prelude.lookup name context of
    Just _ -> fail (AlreadyDeclared name)
    Nothing -> pure ()

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
withStackFrame name (TC action) = TC do
  local (\(Env ctx cs) -> Env ctx (name : cs)) action

withDeclared :: Name -> Type -> TC a -> TC a
withDeclared name type_ = withExtraContext [(name, type_)]

withExtraContext :: [(Name, Type)] -> TC a -> TC a
withExtraContext ctx' (TC action) = TC do
  local (\(Env ctx cs) -> Env (ctx' <> ctx) cs) action

checkExpr :: Expr -> Type -> TC ()
checkExpr expr type_ = do
  actualType <- inferExpr expr
  unless (Value actualType == type_) do
    fail (InvalidTypeFor expr (Actual (Value actualType)) (Expected type_))

withPatternOf :: Type -> Expr -> TC a -> TC a
withPatternOf valueType pat next = do
  case (pat, valueType) of
    (Wildcard name, _) ->
      withDeclared name valueType next
    (App name args, _) -> do
      (argsTypes, resultType) <- lookupForFunction name
      unless (valueType == Value resultType) do
        fail (InvalidTypeFor pat (Actual (Value resultType)) (Expected valueType))
      unless (length args == length argsTypes) do
        fail (InvalidArgumentTypeFor name (Expected argsTypes) (Actual args))
      traverseProduct (zip args (Value <$> argsTypes)) next
    (Tuple args, Value (TupleType argsTypes)) ->
      traverseProduct (zip args (Value <$> argsTypes)) next
    _ -> do
      checkExpr pat valueType
      next
  where
    traverseProduct [] next = next
    traverseProduct ((arg, expectedArgType) : args) next = do
      withPatternOf expectedArgType arg do
        traverseProduct args next

withArguments :: Arguments -> TC a -> TC a
withArguments [] next = next
withArguments ((name_, type_) : args) next = do
  checkExpr type_ (Value Top)
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
--  ──────────
--  n : number
inferExpr (Number _) = pure (Var "number")
--
--  ──────────
--  s : number
inferExpr (String _) = pure (Var "string")
--
--  ─────────────
--  [] : array(⊤)
inferExpr (Array []) = pure Top
--
--    Г ⊢ a₁ : t, …, aᵤ : t
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
  checkExpr elemTypeType (Value Top)
  pure Top
--
--       Г ⊢ a₁ : t₁, …, aᵤ : tᵤ
--  ──────────────────────────────────
--  Г ⊢ (a₁, …, aᵤ) : tuple(t₁, …, tᵤ)
inferExpr (Tuple elems) = TupleType <$> for elems inferExpr
--
--   Г ⊢ t₁ : ⊤, …, tᵤ : ⊤
--  ─────────────────────────
--  Г ⊢ tuple(t₁, …, tᵤ) : ⊤
inferExpr (TupleType elemsTypes) = do
  for elemsTypes \elemType -> do
    checkExpr elemType (Value Top)
  pure Top
--
-- TODO: object type
--
--  ─────────
--  {...} : ⊤
inferExpr (Object _) = pure Top
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
--  Г ⊢ t₁ : ⊤, x : t₁;  Г, a : t₁ ⊢ let as in b : t₂
--  ──────────────────────────────────────────────────
--           Г ⊢ let x : t₁ = a, as in b : t₂
inferExpr (Let ((LetBinding name type_ value) : bindings) next) = do
  withStackFrame (unName name) do
    checkExpr type_ (Value Top)
    checkExpr value (Value type_)
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

checkDeclaration :: Declaration -> TC [(Name, Type)]
checkDeclaration (Definition name args type_ body) = do
  let contextItem = makeContextItem name args type_
  withStackFrame (unName name) do
    withArguments args do
      withDeclared name (snd contextItem) do
        checkExpr body (Value type_)
  pure [contextItem]
checkDeclaration (Inductive name args constructors) = do
  let contextItem = makeContextItem name args Top
  withStackFrame (unName name) do
    withArguments args do
      withDeclared name (snd contextItem) do
        for_ constructors \(Constructor name args) -> do
          withArguments args (pure ())
  let constructorItems = fmap (\(Constructor conName conArgs) -> makeContextItem conName conArgs (Var name)) constructors
  pure (contextItem : constructorItems)
checkDeclaration (External name args type_ _) = do
  withStackFrame (unName name) do
    withArguments args (pure ())
  pure [makeContextItem name args type_]
checkDeclaration (Declare name args type_) = do
  withStackFrame (unName name) do
    withArguments args (pure ())
  pure [makeContextItem name args type_]

checkModule :: [Declaration] -> TC ()
checkModule [] = pure ()
checkModule (decl : decls) = do
  newContext <- checkDeclaration decl
  withExtraContext newContext do
    checkModule decls

defaultContext :: Context
defaultContext =
  [ ("number", Value Top),
    ("string", Value Top),
    ("Record", Function [Top, Top] Top),
    ("the", Function [Top, Var "a"] (Var "a")),
    ("plus", Function [Var "number", Var "number"] (Var "number"))
  ]

check :: [Declaration] -> Maybe (Env, TypeCheckError)
check decls =
  case runReaderT (runTC (checkModule decls)) (Env defaultContext []) of
    Right () -> Nothing
    Left err -> Just err

printCallStack :: CallStack -> Text
printCallStack stack = unlines ("Callstack: " : fmap (\x -> "- " <> show x) stack)

printContext :: Context -> Text
printContext context = "Context: " <> show context

printEnv :: Env -> Text
printEnv (Env context callStack) = printCallStack callStack <> printContext context

printError :: TypeCheckError -> Text
printError err = "TypeCheckError: " <> show err
