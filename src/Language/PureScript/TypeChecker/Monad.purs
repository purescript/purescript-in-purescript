module Language.PureScript.TypeChecker.Monad where

import Language.PureScript.Declarations (canonicalizeDictionary)
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types
import Language.PureScript.Pos

import Data.Array (map)
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Monoid
import Data.Foldable (elem)

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Unsafe
import Control.Monad.Eff.Exception

import Control.Monad.Error

import qualified Data.Map as M

-- |
-- The type checking monad, which provides the state of the type checker, and error reporting capabilities
--
type Check a = Eff (ref :: Ref, err :: Exception ErrorStack) a

-- |
-- Untyped unification variables
--
type Unknown = Number

-- |
-- A type which can contain unification variables
--
class Partial t where
  unknown :: Unknown -> t
  isUnknown :: t -> Maybe Unknown
  unknowns :: t -> [Unknown]
  ($?) :: Substitution t -> t -> t

-- |
-- Identifies types which support unification
--
class (Partial t) <= Unifiable t where
  unify :: RefVal (UnifyState t) -> t -> t -> Check Unit

-- |
-- A substitution maintains a mapping from unification variables to their values
--
data Substitution t = Substitution (Data.Map.Map Unknown t)

runSubstitution :: forall t. Substitution t -> Data.Map.Map Number t
runSubstitution (Substitution m) = m

instance semigroupSubstitution :: (Partial t) => Semigroup (Substitution t) where
  (<>) s1 s2 = Substitution $
                 Data.Map.map (($?) s2) (runSubstitution s1) `Data.Map.union`
                 Data.Map.map (($?) s1) (runSubstitution s2)

instance monoidSubstitution :: (Partial t) => Monoid (Substitution t) where
  mempty = Substitution Data.Map.empty

-- |
-- State required for type checking
--
data UnifyState t = UnifyState (UnifyStateObj t)

type UnifyStateObj t = {
  -- |
  -- The next fresh unification variable
  --
    nextVar :: Unknown
  -- |
  -- The current substitution
  --
  , currentSubstitution :: Substitution t
  }
  
unifyStateObj :: forall t. UnifyState t -> UnifyStateObj t
unifyStateObj (UnifyState o) = o

-- |
-- An empty @UnifyState@
--
defaultUnifyState :: forall t. (Partial t) => UnifyState t
defaultUnifyState = UnifyState { nextVar: 0, currentSubstitution: mempty }

-- |
-- Run a computation in the Unify monad, failing with an error, or succeeding with a return value and the new next unification variable
--
runUnify :: forall t a. UnifyState t -> (RefVal (UnifyState t) -> Check a) -> Check (Tuple a (UnifyState t))
runUnify st unify = do
  stRef <- newRef st
  a <- unify stRef 
  st' <- readRef stRef
  return (Tuple a st')

-- |
-- Substitute a single unification variable
--
substituteOne :: forall t. (Partial t) => Unknown -> t -> Substitution t
substituteOne u t = Substitution $ Data.Map.singleton u t

-- |
-- Replace a unification variable with the specified value in the current substitution
--
substitute :: forall t. (Partial t, Unifiable t) => RefVal (UnifyState t) -> Unknown -> t -> Check Unit
substitute stRef u t' = do
  UnifyState st <- readRef stRef
  let sub = st.currentSubstitution
  let t = sub $? t'
  occursCheck stRef u t
  let current = sub $? unknown u
  case isUnknown current of
    Just u1 | u1 == u -> return unit
    _ -> unify stRef current t
  modifyRef stRef $ \(UnifyState s) -> UnifyState { nextVar: st.nextVar, currentSubstitution: substituteOne u t <> s.currentSubstitution }

-- |
-- Perform the occurs check, to make sure a unification variable does not occur inside a value
--
occursCheck :: forall t. (Partial t) => RefVal (UnifyState t) -> Unknown -> t -> Check Unit
occursCheck stRef u t =
  case isUnknown t of
    Nothing | u `elem` unknowns t -> throwException $ strMsg $ "Occurs check fails"
    _ -> return unit
	
-- |
-- Generate a fresh untyped unification variable
--
fresh' :: forall t. RefVal (UnifyState t) -> Check Unknown
fresh' stRef = do
  UnifyState st <- readRef stRef
  writeRef stRef $ UnifyState 
    { nextVar: st.nextVar + 1
    , currentSubstitution: st.currentSubstitution 
    }
  return st.nextVar

-- |
-- Generate a fresh unification variable at a specific type
--
fresh :: forall t. (Partial t) => RefVal (UnifyState t) -> Check t
fresh stRef = do
  u <- fresh' stRef
  return $ unknown u

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
-- Temporarily bind a collection of names to values
--
bindNames :: forall eff a. RefVal CheckState -> M.Map (Tuple ModuleName Ident) (Tuple Type NameKind) -> Eff (ref :: Ref | eff) a -> Eff (ref :: Ref | eff) a
bindNames stRef newNames action = do
  CheckState orig <- readRef stRef
  modifyRef stRef $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { names = newNames `M.union` (envObj st.env).names } }
  a <- action
  modifyRef stRef $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { names = (envObj orig.env).names } }
  return a

-- |
-- Temporarily bind a collection of names to types
--
bindTypes :: forall eff a. RefVal CheckState -> M.Map (Qualified ProperName) (Tuple Kind TypeKind) -> Eff (ref :: Ref | eff) a -> Eff (ref :: Ref | eff) a
bindTypes stRef newNames action = do
  CheckState orig <- readRef stRef
  modifyRef stRef $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { types = newNames `M.union` (envObj st.env).types } }
  a <- action
  modifyRef stRef $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { types = (envObj orig.env).types } }
  return a

-- |
-- Temporarily make a collection of type class dictionaries available
--
withTypeClassDictionaries :: forall eff a. RefVal CheckState -> [TypeClassDictionaryInScope] -> Eff (ref :: Ref | eff) a -> Eff (ref :: Ref | eff) a
withTypeClassDictionaries stRef entries action = do
  CheckState orig <- readRef stRef
  let mentries = M.fromList $ flip map entries $ \entry -> case entry of
                     TypeClassDictionaryInScope { name = (Qualified mn _) } ->
                       Tuple (Tuple (canonicalizeDictionary entry) mn) entry
  modifyRef stRef $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { typeClassDictionaries = (envObj st.env).typeClassDictionaries `M.union` mentries } }
  a <- action
  modifyRef stRef $ \(CheckState st) -> CheckState $ st { env = Environment $ (envObj st.env) { typeClassDictionaries = (envObj orig.env).typeClassDictionaries } }
  return a

-- |
-- Get the currently available list of type class dictionaries
--
getTypeClassDictionaries :: RefVal CheckState -> Check [TypeClassDictionaryInScope]
getTypeClassDictionaries stRef = M.values <<< (\(CheckState st) -> (envObj st.env).typeClassDictionaries) <$> readRef stRef

-- |
-- Temporarily bind a collection of names to local variables
--
bindLocalVariables :: forall a. RefVal CheckState -> ModuleName -> [Tuple Ident Type] -> Check a -> Check a
bindLocalVariables stRef moduleName bindings =
  bindNames stRef (M.fromList $ flip map bindings $ \(Tuple name ty) -> Tuple (Tuple moduleName name) (Tuple ty LocalVariable))

-- |
-- Temporarily bind a collection of names to local type variables
--
bindLocalTypeVariables :: forall a. RefVal CheckState -> ModuleName -> [Tuple ProperName Kind] -> Check a -> Check a
bindLocalTypeVariables stRef moduleName bindings =
  bindTypes stRef (M.fromList $ flip map bindings $ \(Tuple pn kind) -> Tuple (Qualified (Just moduleName) pn) (Tuple kind LocalTypeVariable))

-- |
-- Lookup the type of a value by name in the @Environment@
--
lookupVariable :: RefVal CheckState -> ModuleName -> Qualified Ident -> Check Type
lookupVariable stRef currentModule (Qualified moduleName var) = do
  Environment env <- getEnv stRef
  case M.lookup (Tuple (fromMaybe currentModule moduleName) var) env.names of
    Nothing -> throwException $ strMsg $ show var ++ " is undefined"
    Just (Tuple ty _) -> return ty

-- |
-- Lookup the kind of a type by name in the @Environment@
--
lookupTypeVariable :: RefVal CheckState -> ModuleName -> Qualified ProperName -> Check Kind
lookupTypeVariable stRef currentModule (Qualified moduleName name) = do
  Environment env <- getEnv stRef
  case M.lookup (Qualified (Just $ fromMaybe currentModule moduleName) name) env.types of
    Nothing -> throwException $ strMsg $ "Type variable " ++ show name ++ " is undefined"
    Just (Tuple k _) -> return k

-- |
-- Get the current @Environment@
--
getEnv :: forall a. RefVal CheckState -> Check Environment
getEnv stRef = (\(CheckState st) -> st.env) <$> readRef stRef

-- |
-- Update the @Environment@
--
putEnv :: forall a. RefVal CheckState -> Environment -> Check Unit
putEnv stRef env = modifyRef stRef $ \(CheckState st) -> CheckState $ st { env = env }

-- |
-- Modify the @Environment@
--
modifyEnv :: forall a. RefVal CheckState -> (Environment -> Environment) -> Check Unit
modifyEnv stRef f = modifyRef stRef (\(CheckState st) -> CheckState $ st { env = f st.env })

-- |
-- Get the current module name
--
getCurrentModule :: forall a. RefVal CheckState -> Check (Maybe ModuleName)
getCurrentModule stRef = (\(CheckState st) -> st.currentModule) <$> readRef stRef

rethrowException :: forall a. (ErrorStack -> ErrorStack) -> Check a -> Check a
rethrowException f = catchException' $ \e -> throwException (f e)

rethrowExceptionWithPosition :: forall a. SourcePos -> Check a -> Check a
rethrowExceptionWithPosition pos = rethrowException ((<>) (positionError pos))

-- |
-- Run a computation in the Check monad, starting with an empty @Environment@
--
runCheck :: forall a. Options -> (RefVal CheckState -> Check a) -> Either String (Tuple a Environment)
runCheck opts = runCheck' opts initEnvironment

unsafeRunEff :: forall eff a. Eff eff a -> a
unsafeRunEff eff = runPure (unsafeInterleaveEff eff)

-- |
-- Run a computation in the Check monad, failing with an error, or succeeding with a return value and the final @Environment@.
--
runCheck' :: forall a. Options -> Environment -> (RefVal CheckState -> Check a) -> Either String (Tuple a Environment)
runCheck' (Options o) env c = stringifyErrorStack o.verboseErrors $ unsafeRunEff $ catchException (pure <<< Left) $ do
  stRef <- newRef (CheckState { env: env, nextVar: 0, nextDictName: 0, currentModule: Nothing })
  a <- c stRef
  CheckState st <- readRef stRef
  return $ Right (Tuple a st.env)

-- |
-- Make an assertion, failing with an error message
--
guardWith :: ErrorStack -> Boolean -> Check Unit
guardWith _ true = return unit
guardWith e false = throwException e

-- |
-- Generate new type class dictionary name
--
freshDictionaryName :: RefVal CheckState -> Check Number
freshDictionaryName stRef = do
  n <- (\(CheckState st) -> st.nextDictName) <$> readRef stRef
  modifyRef stRef $ \(CheckState st) -> CheckState $ st { nextDictName = st.nextDictName + 1 }
  return n

-- |
-- Run a computation in the substitution monad, generating a return value and the final substitution.
--
withSubstitution :: forall t a. (Partial t) => RefVal CheckState -> (RefVal (UnifyState t) -> Check a) -> Check (Tuple a (Substitution t))
withSubstitution stRef unify = do
  CheckState st <- readRef stRef
  Tuple a (UnifyState ust) <- runUnify (UnifyState $ (unifyStateObj defaultUnifyState) { nextVar = st.nextVar }) unify
  modifyRef stRef $ \(CheckState st') -> CheckState $ st' { nextVar = ust.nextVar }
  return $ Tuple a ust.currentSubstitution
