module Language.PureScript.TypeChecker.Monad where

import Language.PureScript.Declarations (canonicalizeDictionary)
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

import Data.Array (map)
import Data.Either
import Data.Maybe
import Data.Tuple

import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Proxy
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Trans
import Control.Monad.Unify

import qualified Data.Map as M

-- |
-- Temporarily bind a collection of names to values
--
bindNames :: forall m a. (Monad m, MonadState CheckState m) => M.Map (Tuple ModuleName Ident) (Tuple Type NameKind) -> m a -> m a
bindNames newNames action = do
  CheckState orig <- get
  modify $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { names = newNames `M.union` (envObj st.env).names } }
  a <- action
  modify $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { names = (envObj orig.env).names } }
  return a

-- |
-- Temporarily bind a collection of names to types
--
bindTypes :: forall m a. (Monad m, MonadState CheckState m) => M.Map (Qualified ProperName) (Tuple Kind TypeKind) -> m a -> m a
bindTypes newNames action = do
  CheckState orig <- get
  modify $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { types = newNames `M.union` (envObj st.env).types } }
  a <- action
  modify $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { types = (envObj orig.env).types } }
  return a

-- |
-- Temporarily make a collection of type class dictionaries available
--
withTypeClassDictionaries :: forall m a. (Monad m, MonadState CheckState m) => [TypeClassDictionaryInScope] -> m a -> m a
withTypeClassDictionaries entries action = do
  CheckState orig <- get
  let mentries = M.fromList $ flip map entries $ \entry -> case entry of
                     TypeClassDictionaryInScope { name = (Qualified mn _) } ->
                       Tuple (Tuple (canonicalizeDictionary entry) mn) entry
  modify $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { typeClassDictionaries = (envObj st.env).typeClassDictionaries `M.union` mentries } }
  a <- action
  modify $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { typeClassDictionaries = (envObj orig.env).typeClassDictionaries } }
  return a

-- |
-- Get the currently available list of type class dictionaries
--
getTypeClassDictionaries :: forall m. (Monad m, MonadState CheckState m) => m [TypeClassDictionaryInScope]
getTypeClassDictionaries = M.values <<< (\(CheckState st) -> (envObj st.env).typeClassDictionaries) <$> get

-- |
-- Temporarily bind a collection of names to local variables
--
bindLocalVariables :: forall m a. (Monad m, MonadState CheckState m) => ModuleName -> [Tuple Ident Type] -> m a -> m a
bindLocalVariables moduleName bindings =
  bindNames (M.fromList $ flip map bindings $ \(Tuple name ty) -> Tuple (Tuple moduleName name) (Tuple ty LocalVariable))

-- |
-- Temporarily bind a collection of names to local type variables
--
bindLocalTypeVariables :: forall m a. (Monad m, MonadState CheckState m) => ModuleName -> [Tuple ProperName Kind] -> m a -> m a
bindLocalTypeVariables moduleName bindings =
  bindTypes (M.fromList $ flip map bindings $ \(Tuple pn kind) -> Tuple (Qualified (Just moduleName) pn) (Tuple kind LocalTypeVariable))

-- |
-- Lookup the type of a value by name in the @Environment@
--
lookupVariable :: forall e m. (Error e, Monad m, MonadState CheckState m, MonadError e m) => WithErrorType e -> ModuleName -> Qualified Ident -> m Type
lookupVariable errorType currentModule (Qualified moduleName var) = do
  Environment env <- getEnv
  case M.lookup (Tuple (fromMaybe currentModule moduleName) var) env.names of
    Nothing -> throwError $ withErrorType errorType $ strMsg $ show var ++ " is undefined"
    Just (Tuple ty _) -> return ty

-- |
-- Lookup the kind of a type by name in the @Environment@
--
lookupTypeVariable :: forall e m. (Error e, Monad m, MonadState CheckState m, MonadError e m) => WithErrorType e -> ModuleName -> Qualified ProperName -> m Kind
lookupTypeVariable errorType currentModule (Qualified moduleName name) = do
  Environment env <- getEnv
  case M.lookup (Qualified (Just $ fromMaybe currentModule moduleName) name) env.types of
    Nothing -> throwError $ withErrorType errorType $ strMsg $ "Type variable " ++ show name ++ " is undefined"
    Just (Tuple k _) -> return k

-- |
-- State required for type checking:
--
data CheckState = CheckState {
  -- |
  -- The current @Environment@
  --
    env :: Environment
  -- |
  -- The next fresh unification variable name
  --
  , nextVar :: Number
  -- |
  -- The next type class dictionary name
  --
  , nextDictName :: Number
  -- |
  -- The current module
  --
  , currentModule :: Maybe ModuleName
  }

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
data Check a = Check (StateT CheckState (Either ErrorStack) a)

unCheck :: forall a. Check a -> StateT CheckState (Either ErrorStack) a
unCheck (Check x) = x

instance functorCheck :: Functor Check where
  (<$>) f (Check x) = Check (f <$> x)

instance applyCheck :: Apply Check where
  (<*>) (Check f) (Check x) = Check (f <*> x)

instance applicativeCheck :: Applicative Check where
  pure a = Check (pure a)

instance bindCheck :: Bind Check where
  (>>=) (Check x) f = Check (x >>= unCheck <<< f)

instance monadCheck :: Monad Check

instance monadErrorCheck :: MonadError ErrorStack Check where
  throwError = Check <<< throwError
  catchError e f = Check $ catchError (unCheck e) (unCheck <<< f)

instance monadStateCheck :: MonadState CheckState Check where
  state = Check <<< state

-- |
-- Get the current @Environment@
--
getEnv :: forall m. (Monad m, MonadState CheckState m) => m Environment
getEnv = (\(CheckState st) -> st.env) <$> get

-- |
-- Update the @Environment@
--
putEnv :: forall m. (Monad m, MonadState CheckState m) => Environment -> m Unit
putEnv env = modify $ \(CheckState st) -> CheckState $ st { env = env }

-- |
-- Modify the @Environment@
--
modifyEnv :: forall m. (Monad m, MonadState CheckState m) => (Environment -> Environment) -> m Unit
modifyEnv f = modify (\(CheckState st) -> CheckState $ st { env = f st.env })

-- |
-- Get the current module name
--
getCurrentModule :: forall m. (Monad m, MonadState CheckState m) => m (Maybe ModuleName)
getCurrentModule = (\(CheckState st) -> st.currentModule) <$> get

-- |
-- Run a computation in the Check monad, starting with an empty @Environment@
--
runCheck :: forall a. Options -> Check a -> Either String (Tuple a Environment)
runCheck opts = runCheck' opts initEnvironment

-- |
-- Run a computation in the Check monad, failing with an error, or succeeding with a return value and the final @Environment@.
--
runCheck' :: forall a. Options -> Environment -> Check a -> Either String (Tuple a Environment)
runCheck' (Options o) env c = stringifyErrorStack o.verboseErrors $ do
  (Tuple a (CheckState st)) <- flip runStateT (CheckState { env: env, nextVar: 0, nextDictName: 0, currentModule: Nothing }) $ unCheck c
  return $ Tuple a (st.env)

-- |
-- Make an assertion, failing with an error message
--
guardWith :: forall e m. (Monad m, MonadError e m) => e -> Boolean -> m Unit
guardWith _ true = return unit
guardWith e false = throwError e

-- |
-- Generate new type class dictionary name
--
freshDictionaryName :: Check Number
freshDictionaryName = do
  n <- (\(CheckState st) -> st.nextDictName) <$> get
  modify $ \(CheckState st) -> CheckState $ st { nextDictName = st.nextDictName + 1 }
  return n

-- |
-- Lift a computation in the @Check@ monad into the substitution monad.
--
liftCheck :: forall a t. Check a -> UnifyT t Check a
liftCheck = UnifyT <<< lift

-- |
-- Run a computation in the substitution monad, generating a return value and the final substitution.
--
liftUnify :: forall t a. (Partial t) => UnifyT t Check a -> Check (Tuple a (Substitution t))
liftUnify unify = do
  CheckState st <- get
  Tuple a (UnifyState ust) <- runUnify (UnifyState $ (unifyStateObj defaultUnifyState) { nextVar = st.nextVar }) unify
  modify $ \(CheckState st') -> CheckState $ st' { nextVar = ust.nextVar }
  return $ Tuple a ust.currentSubstitution
