module Language.PureScript.CodeGen.Externs (moduleToPs) where

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.Writer.Trans
import Control.Monad.Writer.Class

import Data.Array (filter, map, mapMaybe, null)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (elem, find, for_, traverse_)
import Data.Monoid
import Data.String (joinWith)
import Data.Tuple
import Data.Tuple3

import qualified Data.Map as M

import Language.PureScript.Declarations
import Language.PureScript.Environment
import Language.PureScript.Errors (theImpossibleHappened)
import Language.PureScript.Names
import Language.PureScript.Pretty.Kinds
import Language.PureScript.Pretty.Types
import Language.PureScript.TypeClassDictionaries

-- |
-- Generate foreign imports for all declarations in a module
--
moduleToPs :: Module -> Environment -> String
moduleToPs (Module _ _ Nothing) _ = theImpossibleHappened "Module exports were not elaborated in moduleToPs"
moduleToPs (Module moduleName ds (Just exts)) (Environment env) = joinWith "\n" <<< execWriter $ do
  tell [ "module " ++ runModuleName moduleName ++ " where"]
  traverse_ declToPs ds
  traverse_ exportToPs exts
  where

    declToPs :: Declaration -> Writer [String] {}
    declToPs (ImportDeclaration mn _ _) = tell ["import " ++ show mn ++ " ()"]
    declToPs (FixityDeclaration (Fixity assoc prec) ident) =
      tell [ joinWith " " [ show assoc, show prec, ident ] ]
    declToPs (PositionedDeclaration _ d) = declToPs d
    declToPs _ = return {}

    exportToPs :: DeclarationRef -> Writer [String] {}
    exportToPs (PositionedDeclarationRef _ r) = exportToPs r
    exportToPs (TypeRef pn dctors) =
      case Qualified (Just moduleName) pn `M.lookup` env.types of
        Nothing -> theImpossibleHappened $ show pn ++ " has no kind in exportToPs"
        Just (Tuple kind ExternData) ->
          tell ["foreign import data " ++ show pn ++ " :: " ++ prettyPrintKind kind]
        Just (Tuple _ (DataType args tys)) -> do
          let dctors' = fromMaybe (map fst tys) dctors
              printDctor dctor = case dctor `lookup` tys of
                                   Nothing -> Nothing
                                   Just tyArgs -> Just $ show dctor ++ " " ++ joinWith " " (map prettyPrintTypeAtom tyArgs)
          tell ["data " ++ show pn ++ " " ++ joinWith " " args ++ (if null dctors' then "" else " = " ++ joinWith " | " (mapMaybe printDctor dctors'))]
        Just (Tuple _ TypeSynonym) ->
          case Qualified (Just moduleName) pn `M.lookup` env.typeSynonyms of
            Nothing -> theImpossibleHappened $ show pn ++ " has no type synonym info in exportToPs"
            Just (Tuple args synTy) ->
              tell ["type " ++ show pn ++ " " ++ joinWith " " args ++ " = " ++ prettyPrintType synTy]
        _ -> theImpossibleHappened "Invalid input in exportToPs"

    exportToPs (ValueRef ident) =
      case (Tuple moduleName ident) `M.lookup` env.names of
        Nothing -> theImpossibleHappened $ show ident ++ " has no type in exportToPs"
        Just (Tuple ty nameKind) | nameKind == Value || nameKind == Extern ForeignImport || nameKind == Extern InlineJavascript ->
          tell ["foreign import " ++ show ident ++ " :: " ++ prettyPrintType ty]
        _ -> return {}
    exportToPs (TypeClassRef className) =
      case Qualified (Just moduleName) className `M.lookup` env.typeClasses of
        Nothing -> theImpossibleHappened $ show className ++ " has no type class definition in exportToPs"
        Just (Tuple3 args members implies) -> do
          let impliesString = if null implies then "" else "(" ++ joinWith ", " (map (\(Tuple pn tys') -> show pn ++ " " ++ joinWith " " (map prettyPrintTypeAtom tys')) implies) ++ ") <= "
          let exportedMembers = filter (isValueExported <<< fst) members
          tell ["class " ++ impliesString ++ show className ++ " " ++ joinWith " " args ++ (if null exportedMembers then "" else " where")]
          for_ exportedMembers $ \(Tuple member ty) ->
            tell [ "  " ++ show member ++ " :: " ++ prettyPrintType ty ]

    exportToPs (TypeInstanceRef ident) = do
      case find (((==) (Qualified (Just moduleName) ident)) <<< (\(TypeClassDictionaryInScope tcd) -> tcd.name)) $ M.values env.typeClassDictionaries of
        Nothing -> theImpossibleHappened "Type class instance has no dictionary in exportToPs"
        Just (TypeClassDictionaryInScope tcd) -> do
          let constraintsText = case fromMaybe [] tcd.dependencies of
                                  [] -> ""
                                  cs -> "(" ++ joinWith ", " (map (\(Tuple pn tys') -> show pn ++ " " ++ joinWith " " (map prettyPrintTypeAtom tys')) cs) ++ ") => "
          tell ["foreign import instance " ++ show ident ++ " :: " ++ constraintsText ++ show tcd.className ++ " " ++ joinWith " " (map prettyPrintTypeAtom tcd.instanceTypes)]

    isValueExported :: Ident -> Boolean
    isValueExported ident = ValueRef ident `elem` exts
    
    -- TODO: put this somewhere else
    lookup :: forall a b. (Eq a) => a -> [Tuple a b] -> Maybe b
    lookup key ((Tuple x y) : xys) = if key == x then Just y else lookup key xys
    lookup _ [] = Nothing
