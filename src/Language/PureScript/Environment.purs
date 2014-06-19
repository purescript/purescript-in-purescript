module Language.PureScript.Environment where

import Data.Maybe
import Data.Tuple
import Data.Tuple3

import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types

import qualified Language.PureScript.Constants as C
import qualified Data.Map as M

-- |
-- The @Environment@ defines all values and types which are currently in scope:
--
data Environment = Environment EnvironmentObj

type EnvironmentObj = {
  -- |
  -- Value names currently in scope
  --
    names :: M.Map (Tuple ModuleName Ident) (Tuple Type NameKind)
  -- |
  -- Type names currently in scope
  --
  , types :: M.Map (Qualified ProperName) (Tuple Kind TypeKind)
  -- |
  -- Data constructors currently in scope, along with their associated data type constructors
  --
  , dataConstructors :: M.Map (Qualified ProperName) (Tuple ProperName Type)
  -- |
  -- Type synonyms currently in scope
  --
  , typeSynonyms :: M.Map (Qualified ProperName) (Tuple [String] Type)
  -- |
  -- Available type class dictionaries
  --
  , typeClassDictionaries :: M.Map (Tuple (Qualified Ident) (Maybe ModuleName)) TypeClassDictionaryInScope
  -- |
  -- Type classes
  --
  , typeClasses :: M.Map (Qualified ProperName) (Tuple3 [String] [Tuple Ident Type] [Tuple (Qualified ProperName) [Type]])
  }

envObj :: Environment -> EnvironmentObj
envObj (Environment o) = o

instance showEnv :: Show Environment where
  show (Environment o) = "Environment {" ++
    "names: " ++ show o.names ++ ", " ++
    "types: " ++ show o.types ++ ", " ++
    "dataConstructors: " ++ show o.dataConstructors ++ ", " ++
    "typeSynonyms: " ++ show o.typeSynonyms ++ ", " ++
    "typeClassDictionaries: " ++ show o.typeClassDictionaries ++ ", " ++
    "typeClasses: " ++ show o.typeClasses ++ " " ++
    "}"

-- |
-- The initial environment with no values and only the default javascript types defined
--
initEnvironment :: Environment
initEnvironment = Environment { names: M.empty
                              , types: primTypes
                              , dataConstructors: M.empty
                              , typeSynonyms: M.empty
                              , typeClassDictionaries: M.empty
                              , typeClasses: M.empty
                              }

-- |
-- The type of a foreign import
--
data ForeignImportType
  -- |
  -- A regular foreign import
  --
  = ForeignImport
  -- |
  -- A foreign import which contains inline Javascript as a string literal
  --
  | InlineJavascript

instance showForeignImport :: Show ForeignImportType where
  show ForeignImport = "ForeignImport"
  show InlineJavascript = "InlineJavascript"

instance eqForeignImport :: Eq ForeignImportType where
  (==) ForeignImport           ForeignImport           = true
  (==) InlineJavascript        InlineJavascript        = true
  (==) _ _ = false
  (/=) x y = not (x == y)

-- |
-- The kind of a name
--
data NameKind
  -- |
  -- A value introduced as a binding in a module
  --
  = Value
  -- |
  -- A foreign import
  --
  | Extern ForeignImportType
  -- |
  -- A local name introduced using a lambda abstraction, variable introduction or binder
  --
  | LocalVariable
  -- |
  -- A data constructor
  --
  | DataConstructor
  -- |
  -- A type class dictionary, generated during desugaring of type class declarations
  --
  | TypeInstanceDictionaryValue
  -- |
  -- A type instance member, generated during desugaring of type class declarations
  --
  | TypeInstanceMember
  -- |
  -- A type class dictionary member accessor import, generated during desugaring of type class declarations
  --
  | TypeClassAccessorImport

instance showNameKind :: Show NameKind where
  show Value = "Value"
  show (Extern fit) = "Extern (" ++ show fit ++ ")"
  show LocalVariable = "LocalVariable"
  show DataConstructor = "DataConstructor"
  show TypeInstanceDictionaryValue = "TypeInstanceDictionaryValue"
  show TypeInstanceMember = "TypeInstanceMember"
  show TypeClassAccessorImport = "TypeClassAccessorImport"

instance eqNameKind :: Eq NameKind where
  (==) Value                       Value                       = true
  (==) (Extern t1)                 (Extern t2)                 = t1 == t2
  (==) LocalVariable               LocalVariable               = true
  (==) DataConstructor             DataConstructor             = true
  (==) TypeInstanceDictionaryValue TypeInstanceDictionaryValue = true
  (==) TypeInstanceMember          TypeInstanceMember          = true
  (==) TypeClassAccessorImport     TypeClassAccessorImport     = true
  (==) _ _ = false
  (/=) x y = not (x == y)

-- |
-- The kinds of a type
--
data TypeKind
  -- |
  -- Data type
  --
  = DataType [String] [Tuple ProperName [Type]]
  -- |
  -- Type synonym
  --
  | TypeSynonym
  -- |
  -- Foreign data
  --
  | ExternData
  -- |
  -- A local type variable
  --
  | LocalTypeVariable

instance showTypeKind :: Show TypeKind where
  show (DataType args dctors) = "DataType (" ++ show args ++ ") (" ++ show dctors ++ ")"
  show TypeSynonym = "TypeSynonym"
  show ExternData = "ExternData"
  show LocalTypeVariable = "LocalTypeVariable"

instance eqTypeKind :: Eq TypeKind where
  (==) (DataType args1 tys1) (DataType args2 tys2) = args1 == args2 && tys1 == tys2
  (==) TypeSynonym           TypeSynonym           = true
  (==) ExternData            ExternData            = true
  (==) LocalTypeVariable     LocalTypeVariable     = true
  (==) _ _ = false
  (/=) x y = not (x == y)

-- |
-- Construct a ProperName in the Prim module
--
primName :: String -> Qualified ProperName
primName = Qualified (Just $ ModuleName [ProperName C.prim]) <<< ProperName

-- |
-- Construct a type in the Prim module
--
primTy :: String -> Type
primTy = TypeConstructor <<< primName

-- |
-- Type constructor for functions
--
tyFunction :: Type
tyFunction = primTy "Function"

-- |
-- Type constructor for strings
--
tyString :: Type
tyString = primTy "String"

-- |
-- Type constructor for numbers
--
tyNumber :: Type
tyNumber = primTy "Number"

-- |
-- Type constructor for booleans
--
tyBoolean :: Type
tyBoolean = primTy "Boolean"

-- |
-- Type constructor for arrays
--
tyArray :: Type
tyArray = primTy "Array"

-- |
-- Type constructor for objects
--
tyObject :: Type
tyObject = primTy "Object"

-- |
-- Smart constructor for function types
--
function :: Type -> Type -> Type
function t1 = TypeApp (TypeApp tyFunction t1)

-- |
-- The primitive types in the external javascript environment with their associated kinds.
--
primTypes :: M.Map (Qualified ProperName) (Tuple Kind TypeKind)
primTypes = M.fromList [ Tuple (primName "Function") (Tuple (FunKind Star (FunKind Star Star)) ExternData)
                       , Tuple (primName "Array")    (Tuple (FunKind Star Star) ExternData)
                       , Tuple (primName "Object")   (Tuple (FunKind (Row Star) Star) ExternData)
                       , Tuple (primName "String")   (Tuple Star ExternData)
                       , Tuple (primName "Number")   (Tuple Star ExternData)
                       , Tuple (primName "Boolean")  (Tuple Star ExternData) ]
