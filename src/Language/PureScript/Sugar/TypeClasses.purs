-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeClasses
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which creates type synonyms for type class dictionaries
-- and dictionary expressions for type class instances.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.TypeClasses (
    desugarTypeClasses
  ) where

import Data.Tuple
import Data.Array
import Data.Maybe
import Data.Either
import Data.Foldable (foldl, lookup)
import Data.Traversable (traverse)
import Data.String (joinWith)

import qualified Data.Map as M

import Control.Apply
import Control.Arrow (first, second)

import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.State.Trans
import Control.Monad.State.Class

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Sugar.CaseDeclarations
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Supply
import Language.PureScript.Pretty.Types (prettyPrintTypeAtom)

import qualified Language.PureScript.Constants as C

type MemberMap = M.Map (Tuple ModuleName ProperName) Declaration

type Desugar = StateT MemberMap (SupplyT (Either ErrorStack))

-- |
-- Add type synonym declarations for type class dictionary types, and value declarations for type class
-- instance dictionary expressions.
--
desugarTypeClasses :: [Module] -> SupplyT (Either ErrorStack) [Module]
desugarTypeClasses = flip evalStateT M.empty <<< traverse desugarModule

desugarModule :: Module -> Desugar Module
desugarModule (Module name decls (Just exps)) = do
  (Tuple newExpss declss) <- unzip <$> traverse (desugarDecl name) decls
  return $ Module name (concat declss) $ Just (exps ++ catMaybes newExpss)
desugarModule _ = theImpossibleHappened "Exports should have been elaborated in name desugaring"

{- Desugar type class and type class instance declarations
--
-- Type classes become type synonyms for their dictionaries, and type instances become dictionary declarations.
-- Additional values are generated to access individual members of a dictionary, with the appropriate type.
--
-- E.g. the following
--
--   module Test where
--
--   class Foo a where
--     foo :: a -> a
--
--   instance fooString :: Foo String where
--     foo s = s ++ s
--
--   instance fooArray :: (Foo a) => Foo [a] where
--     foo = map foo
--
--   {- Superclasses -}
--
--   class (Foo a) <= Sub a where
--     sub :: a
--
--   instance subString :: Sub String where
--     sub = ""
--
-- becomes
--
--   type Foo a = { foo :: a -> a }
--
--   foreign import foo "function foo(dict) {\
--                      \  return dict.foo;\
--                      \}" :: forall a. (Foo a) => a -> a
--
--   fooString :: {} -> Foo String
--   fooString _ = { foo: \s -> s ++ s }
--
--   fooArray :: forall a. (Foo a) => Foo [a]
--   fooArray = { foo: map foo }
--
--   {- Superclasses -}
--
--   ...
--
--   subString :: {} -> { __superclasses :: { "Foo": {} -> Foo String }, sub :: String }
--   subString _ = {
--     __superclasses: {
--       "Foo": \_ -> <dictionary placeholder to be inserted during type checking\>
--     }
--     sub: ""
--   }
-}
desugarDecl :: ModuleName -> Declaration -> Desugar (Tuple (Maybe DeclarationRef) [Declaration])
desugarDecl mn d@(TypeClassDeclaration name args implies members) = do
  modify (M.insert (Tuple mn name) d)
  return $ (Tuple Nothing (d : typeClassDictionaryDeclaration name args implies members : map (typeClassMemberToDictionaryAccessor mn name args) members))
desugarDecl mn d@(TypeInstanceDeclaration name deps className ty members) = do
  desugared <- lift $ desugarCases members
  dictDecl <- typeInstanceDictionaryDeclaration name mn deps className ty desugared
  return $ (Tuple (Just (TypeInstanceRef name)) [d, dictDecl])
desugarDecl mn (PositionedDeclaration pos d) = do
  (Tuple dr ds) <- rethrowWithPosition pos $ desugarDecl mn d
  return (Tuple dr (map (PositionedDeclaration pos) ds))
desugarDecl _ other = return (Tuple Nothing [other])

memberToNameAndType :: Declaration -> (Tuple Ident Type)
memberToNameAndType (TypeDeclaration ident ty) = (Tuple ident ty)
memberToNameAndType (PositionedDeclaration _ d) = memberToNameAndType d
memberToNameAndType _ = theImpossibleHappened "Invalid declaration in type class definition"

identToProperty :: Ident -> String
identToProperty (Ident name) = name
identToProperty (Op op) = op

typeClassDictionaryDeclaration :: ProperName -> [String] -> [Tuple (Qualified ProperName) [Type]] -> [Declaration] -> Declaration
typeClassDictionaryDeclaration name args implies members =
  let rowProperties = 
    do Tuple index (Tuple superclass tyArgs) <- zip (range 0 (length implies - 1)) implies
       let tySynApp = foldl TypeApp (TypeConstructor superclass) tyArgs
       let fieldName = mkSuperclassDictionaryName superclass index
       return (Tuple fieldName (function unitType tySynApp)) in
  let superclassesType = TypeApp tyObject (rowFromList (Tuple rowProperties REmpty))
  in TypeSynonymDeclaration name args (TypeApp tyObject $ rowFromList (Tuple ((Tuple C.__superclasses superclassesType) : map (first identToProperty <<< memberToNameAndType) members) REmpty))

typeClassMemberToDictionaryAccessor :: ModuleName -> ProperName -> [String] -> Declaration -> Declaration
typeClassMemberToDictionaryAccessor mn name args (TypeDeclaration ident ty) =
  ValueDeclaration ident TypeClassAccessorImport [] Nothing $
    TypedValue false (Abs (Left $ Ident "dict") (Accessor (runIdent ident) (Var $ Qualified Nothing (Ident "dict")))) $
    moveQuantifiersToFront (quantify (ConstrainedType [Tuple (Qualified (Just mn) name) (map TypeVar args)] ty))
typeClassMemberToDictionaryAccessor mn name args (PositionedDeclaration pos d) =
  PositionedDeclaration pos $ typeClassMemberToDictionaryAccessor mn name args d
typeClassMemberToDictionaryAccessor _ _ _ _ = theImpossibleHappened "Invalid declaration in type class definition"

mkSuperclassDictionaryName :: Qualified ProperName -> Number -> String
mkSuperclassDictionaryName pn index = show pn ++ "_" ++ show index

unitType :: Type
unitType = TypeApp tyObject REmpty

typeInstanceDictionaryDeclaration :: Ident -> ModuleName -> [Tuple (Qualified ProperName) [Type]] -> Qualified ProperName -> [Type] -> [Declaration] -> Desugar Declaration
typeInstanceDictionaryDeclaration name mn deps className tys decls =
  rethrow (\e -> strMsg ("Error in type class instance " ++ show className ++ " " ++ joinWith " " (map prettyPrintTypeAtom tys) ++ ": ") <> (e :: ErrorStack)) $ do
  m <- get

  -- Lookup the type arguments and member types for the type class
  (TypeClassDeclaration _ args implies tyDecls) <- lift <<< lift $
    maybe (Left $ mkErrorStack ("Type class " ++ show className ++ " is undefined") Nothing) Right $
      M.lookup (qualify mn className) m

  case mapMaybe declName tyDecls \\ mapMaybe declName decls of
    x : _ -> throwError $ mkErrorStack ("Member '" ++ show x ++ "' has not been implemented") Nothing
    [] -> do

      let instanceTys = map memberToNameAndType tyDecls

      -- Replace the type arguments with the appropriate types in the member types
      let memberTypes = map (second (replaceAllTypeVars (zip args tys))) instanceTys
      -- Create values for the type instance members
      memberNames <- map (first identToProperty) <$> traverse (memberToNameAndValue memberTypes) decls
      -- Create the type of the dictionary
      -- The type is an object type, but depending on type instance dependencies, may be constrained.
      -- The dictionary itself is an object literal, but for reasons related to recursion, the dictionary
      -- must be guarded by at least one function abstraction. For that reason, if the dictionary has no
      -- dependencies, we introduce an unnamed function parameter.
      let superclasses = ObjectLiteral $ 
        do (Tuple index (Tuple superclass suTyArgs)) <- zip (range 0 (length implies - 1)) implies
           let tyArgs = map (replaceAllTypeVars (zip args tys)) suTyArgs
           let fieldName = mkSuperclassDictionaryName superclass index
           return (Tuple fieldName (Abs (Left (Ident "_")) (SuperClassDictionary superclass tyArgs)))

      let memberNames' = (Tuple C.__superclasses superclasses) : memberNames
          dictTy = foldl TypeApp (TypeConstructor className) tys
          constrainedTy = quantify (if null deps then function unitType dictTy else ConstrainedType deps dictTy)
          dict = if null deps then Abs (Left (Ident "_")) (ObjectLiteral memberNames') else ObjectLiteral memberNames'

      return $ ValueDeclaration name TypeInstanceDictionaryValue [] Nothing (TypedValue true dict constrainedTy)

  where

  declName :: Declaration -> Maybe Ident
  declName (PositionedDeclaration _ d) = declName d
  declName (ValueDeclaration ident _ _ _ _) = Just ident
  declName (TypeDeclaration ident _) = Just ident
  declName _ = Nothing

  memberToNameAndValue :: [Tuple Ident Type] -> Declaration -> Desugar (Tuple Ident Value)
  memberToNameAndValue tys' d@(ValueDeclaration ident _ _ _ _) = do
    lift <<< lift <<< maybe (Left $ mkErrorStack ("Type class does not define member '" ++ show ident ++ "'") Nothing) Right $ lookup ident tys'
    let memberValue = typeInstanceDictionaryEntryValue d
    return (Tuple ident memberValue)
  memberToNameAndValue tys' (PositionedDeclaration pos d) = rethrowWithPosition pos $ do
    (Tuple ident val) <- memberToNameAndValue tys' d
    return (Tuple ident (PositionedValue pos val))
  memberToNameAndValue _ _ = theImpossibleHappened "Invalid declaration in type instance definition"

  typeInstanceDictionaryEntryValue :: Declaration -> Value
  typeInstanceDictionaryEntryValue (ValueDeclaration _ _ [] _ val) = val
  typeInstanceDictionaryEntryValue (PositionedDeclaration pos d) = PositionedValue pos (typeInstanceDictionaryEntryValue d)
  typeInstanceDictionaryEntryValue _ = theImpossibleHappened "Invalid declaration in type instance definition"
  