module Language.PureScript.Environment where

import Data.Maybe
import Data.Tuple

import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Show
import Language.PureScript.Values

import qualified Language.PureScript.Constants as C
import qualified Data.Map as M

-- |
-- The @Environment@ defines all values and types which are currently in scope:
--
data Environment = Environment {
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
  , typeClassDictionaries :: [TypeClassDictionaryInScope]
  -- |
  -- Type classes
  --
  , typeClasses :: M.Map (Qualified ProperName) (Tuple [String] [Tuple Ident Type])
  }
  
instance showEnv :: Show Environment where
  show = defaultShow

-- |
-- The initial environment with no values and only the default javascript types defined
--
initEnvironment :: Environment
initEnvironment = Environment { names: M.empty
                              , types: primTypes
                              , dataConstructors: M.empty
                              , typeSynonyms: M.empty
                              , typeClassDictionaries: []
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
  -- |
  -- A type class dictionary member accessor import, generated during desugaring of type class declarations
  --
  | TypeClassAccessorImport
  
instance showForeignImport :: Show ForeignImportType where
  show = defaultShow
  
instance eqForeignImport :: Eq ForeignImportType where
  (==) ForeignImport           ForeignImport           = true
  (==) InlineJavascript        InlineJavascript        = true
  (==) TypeClassAccessorImport TypeClassAccessorImport = true
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
  
instance showNameKind :: Show NameKind where
  show = defaultShow
  
instance eqNameKind :: Eq NameKind where
  (==) Value                       Value                       = true
  (==) (Extern t1)                 (Extern t2)                 = t1 == t2
  (==) LocalVariable               LocalVariable               = true
  (==) DataConstructor             DataConstructor             = true
  (==) TypeInstanceDictionaryValue TypeInstanceDictionaryValue = true
  (==) TypeInstanceMember          TypeInstanceMember          = true
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
  show = defaultShow
  
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
                       , Tuple (primName "String")   (Tuple Star ExternData)
                       , Tuple (primName "Number")   (Tuple Star ExternData)
                       , Tuple (primName "Boolean")  (Tuple Star ExternData) ]
