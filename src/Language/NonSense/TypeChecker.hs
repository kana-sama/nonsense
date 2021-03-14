{-# LANGUAGE TypeApplications #-}

module Language.NonSense.TypeChecker where

import qualified Data.Generics.Uniplate.Data as Uniplate
import Language.NonSense.AST
import NSPrelude
import qualified Prelude (lookup)

newtype Expected a = Expected a
  deriving stock (Show)

newtype Actual a = Actual a
  deriving stock (Show)

data TypeCheckError
  = UnknownVariable Name
  | InvalidArgumentTypeFor Name (Expected [Expr]) (Actual [Expr])
  | FunctionUsedAsValue Name
  | ValueUsedAsFunction Name
  | InvalidTypeFor Name (Expected Expr) (Actual Expr)
  | InvalidPatternsType (Expected Expr) (Actual Expr)
  | InvalidBranchesTypes (Actual [Expr])
  | AlreadyDeclared Name
  | TypeIsNotTypeFor Name (Actual Expr)
  | InvalidArgumentsCounts Name (Expected Int) (Actual Int)
  deriving stock (Show)

data Type
  = Function [Expr] Expr
  | Value Expr
  deriving stock (Eq, Show)

type Context = [(Name, Type)]

type CallStack = [Text]

newtype TC a = TC {runTC :: StateT (Context, CallStack) (Either (CallStack, TypeCheckError)) a}
  deriving newtype (Functor, Applicative, Monad)

getContext :: TC Context
getContext = TC (gets fst)

getCallStack :: TC CallStack
getCallStack = TC (gets snd)

fail :: TypeCheckError -> TC a
fail error = do
  callStack <- getCallStack
  TC (lift (Left (callStack, error)))

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

addStackFrame :: Text -> TC ()
addStackFrame name = TC (modify \(ctx, cs) -> (ctx, name : cs))

removeStackFrame :: TC ()
removeStackFrame = TC (modify \(ctx, cs) -> (ctx, tail cs))

withStackFrame :: Text -> TC a -> TC a
withStackFrame name action = do
  addStackFrame name
  result <- action
  removeStackFrame
  pure result

declare :: Name -> Type -> TC ()
declare name type_ = TC (modify \(ctx, cs) -> ((name, type_) : ctx, cs))

undeclareLast :: TC ()
undeclareLast = TC (modify \(ctx, cs) -> (tail ctx, cs))

declareGlobal :: Name -> Type -> TC ()
declareGlobal name type_ = do
  checkUndeclared name
  declare name type_

declareLocals :: [(Name, Type)] -> TC a -> TC a
declareLocals binds action = do
  for binds \(name, type_) -> declare name type_
  result <- action
  for binds \_ -> undeclareLast
  pure result

declareLocal :: Name -> Type -> TC a -> TC a
declareLocal name type_ = declareLocals [(name, type_)]

declareDefinitionWithArgs :: Name -> [Expr] -> Expr -> TC ()
declareDefinitionWithArgs name [] type_ =
  declareGlobal name (Value type_)
declareDefinitionWithArgs name args type_ =
  declareGlobal name (Function args type_)

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

checkSameType :: TypeCheckError -> [Expr] -> TC Expr
checkSameType err [] = pure U
checkSameType err (type_ : types) = do
  unless (all (== type_) types) do
    fail err
  pure type_

checkLetBinding :: LetBinding -> TC ()
checkLetBinding (LetBinding name type_ value) = do
  valueType <- inferExpr value
  unless (valueType == type_) do
    fail (InvalidTypeFor name (Expected type_) (Actual valueType))

inferExpr :: Expr -> TC Expr
inferExpr (Var name) = lookupForValue name
inferExpr (App fun args) = do
  argTypes <- for args inferExpr
  (formalArgsTypes, resultType) <- lookupForFunction fun
  unless (formalArgsTypes == argTypes) do
    fail (InvalidArgumentTypeFor fun (Expected formalArgsTypes) (Actual argTypes))
  pure resultType
inferExpr (Number _) = pure (Var "number")
inferExpr (String _) = pure (Var "string")
inferExpr (Array []) = pure U
inferExpr (Array elems) = do
  elemTypes <- for elems inferExpr
  if allSame elemTypes
    then pure (ArrayType (head elemTypes))
    else pure (Array elemTypes)
inferExpr (Object _) = pure U
inferExpr (Wildcard _) = pure U
inferExpr (Match value cases) = do
  valueType <- inferExpr value
  branchTypes <- for cases \(pat, branch) -> do
    case pat of
      -- TODO: bullstit
      App conName conArgs -> do
        let wildcards = unWildcard <$> Uniplate.universeBi @_ @Wildcard conArgs
        (formalArgs, formalType) <- lookupForFunction conName
        unless (length wildcards == length formalArgs) do
          fail (InvalidArgumentsCounts conName (Expected (length formalArgs)) (Actual (length wildcards)))
        unless (formalType == valueType) do
          fail (InvalidPatternsType (Expected valueType) (Actual formalType))
        declareLocals (zip wildcards (Value <$> formalArgs)) (inferExpr branch)
      _ -> inferExpr branch
  checkSameType (InvalidBranchesTypes (Actual branchTypes)) branchTypes
inferExpr (Let bindings next) = go bindings
  where
    go [] = inferExpr next
    go (binding : bindings) = do
      checkLetBinding binding
      let LetBinding name type_ _ = binding
      declareLocal name (Value type_) (go bindings)
inferExpr U = pure U

checkExpr :: Expr -> TC ()
checkExpr = void . inferExpr

checkConstructor :: Name -> Constructor -> TC ()
checkConstructor typeName (Constructor name args) = do
  _ <- inferExprWithLocals args U
  case args of
    [] -> declareGlobal name (Value (Var typeName))
    _ -> declareGlobal name (Function (snd <$> args) (Var typeName))

inferExprWithLocals :: [(Name, Expr)] -> Expr -> TC Expr
inferExprWithLocals [] expr = inferExpr expr
inferExprWithLocals ((name, type_) : binds) expr = do
  typeType <- inferExpr type_
  unless (typeType == U) do fail (TypeIsNotTypeFor name (Actual typeType))
  declareLocal name (Value type_) (inferExprWithLocals binds expr)

checkDeclaration :: Declaration -> TC ()
checkDeclaration (Definition name args type_ body) = do
  declareDefinitionWithArgs name (snd <$> args) type_
  withStackFrame (unName name) do
    bodyType <- (inferExprWithLocals args body)
    unless (bodyType == type_) do
      fail (InvalidTypeFor name (Expected type_) (Actual bodyType))
checkDeclaration (Inductive name args constructors) = do
  declareDefinitionWithArgs name (snd <$> args) U
  withStackFrame (unName name) do
    for_ constructors (checkConstructor name)
checkDeclaration (External name args Nothing _body) = do
  _ <- inferExprWithLocals args U
  declareDefinitionWithArgs name (snd <$> args) U
checkDeclaration (External name args (Just type_) _body) = do
  _ <- inferExprWithLocals args U
  declareDefinitionWithArgs name (snd <$> args) type_

checkModule :: [Declaration] -> TC ()
checkModule decls = for_ decls checkDeclaration

defaultContext :: Context
defaultContext =
  [ ("number", Value U),
    ("string", Value U),
    ("Record", Function [U, U] U)
  ]

check :: [Declaration] -> Maybe (CallStack, TypeCheckError)
check decls =
  case evalStateT (runTC (checkModule decls)) (defaultContext, []) of
    Right () -> Nothing
    Left err -> Just err
