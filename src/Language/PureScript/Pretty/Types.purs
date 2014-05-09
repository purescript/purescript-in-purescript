module Language.PureScript.Pretty.Types
  ( prettyPrintType
  , prettyPrintTypeAtom
  , prettyPrintRow
  ) where

import Control.Arrow ((<+>))
import Data.Array (map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.Tuple
import Text.Pretty.PatternArrows

import Language.PureScript.Environment
import Language.PureScript.Pretty.Common
import Language.PureScript.Types
import Language.PureScript.Names

typeLiterals :: Unit -> Pattern Unit Type String
typeLiterals _ = mkPattern match
  where
  match (TypeVar var) = Just var
  match (PrettyPrintObject row) = Just $ "{ " ++ prettyPrintRow row ++ " }"
  match (PrettyPrintArray ty) = Just $ "[" ++ prettyPrintType ty ++ "]"
  match (TypeConstructor ctor) = Just $ show ctor
  match (TUnknown u) = Just $ "u" ++ show u
  match (Skolem name s _) = Just $ name ++ show s
  match (ConstrainedType deps ty) = Just $ "(" ++ joinWith ", " (map (\(Tuple pn ty') -> show pn ++ " " ++ joinWith " " (map prettyPrintTypeAtom ty')) deps) ++ ") => " ++ prettyPrintType ty
  match (SaturatedTypeSynonym name args) = Just $ show name ++ "<" ++ joinWith "," (map prettyPrintTypeAtom args) ++ ">"
  match REmpty = Just "()"
  match row@(RCons _ _ _) = Just $ "(" ++ prettyPrintRow row ++ ")"
  match _ = Nothing

typeApp :: Pattern Unit Type (Tuple Type Type)
typeApp = mkPattern match
  where
  match (TypeApp f x) = Just (Tuple f x)
  match _ = Nothing

appliedFunction :: Pattern Unit Type (Tuple Type Type)
appliedFunction = mkPattern match
  where
  match (PrettyPrintFunction arg ret) = Just (Tuple arg ret)
  match _ = Nothing

insertPlaceholders :: Type -> Type
insertPlaceholders = everywhereOnTypesTopDown convertForAlls <<< everywhereOnTypes convert
  where
  convert (TypeApp (TypeApp f arg) ret) | f == tyFunction = PrettyPrintFunction arg ret
  convert (TypeApp a el) | a == tyArray = PrettyPrintArray el
  convert (TypeApp o r) | o == tyObject = PrettyPrintObject r
  convert other = other
  convertForAlls (ForAll ident ty _) = go [ident] ty
    where
    go idents (ForAll ident' ty' _) = go (ident' : idents) ty'
    go idents other = PrettyPrintForAll idents other
  convertForAlls other = other

matchTypeAtom :: Unit -> Pattern Unit Type String
matchTypeAtom _ = typeLiterals unit <+> (parens <$> matchType unit)

matchType :: Unit -> Pattern Unit Type String
matchType _ = fix $ \p -> buildPrettyPrinter operators (typeLiterals unit <+> (parens <$> p))
  where
  operators :: OperatorTable Unit Type String
  operators =
    OperatorTable [ [ Operator (assocL typeApp $ \f x -> f ++ " " ++ x) ]
                  , [ Operator (assocR appliedFunction $ \arg ret -> arg ++ " -> " ++ ret) ]
                  , [ Operator (wrap forall_ $ \idents ty -> "forall " ++ joinWith " " idents ++ ". " ++ ty) ]
                  ]

forall_ :: Pattern Unit Type (Tuple [String] Type)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty) = Just (Tuple idents ty)
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing a Type, as it should appear inside parentheses
--
prettyPrintTypeAtom :: Type -> String
prettyPrintTypeAtom p = runPretty (pattern (matchTypeAtom unit) unit <<< insertPlaceholders) p

-- |
-- Generate a pretty-printed string representing a Type
--
prettyPrintType :: Type -> String
prettyPrintType p = runPretty (pattern (matchType unit) unit <<< insertPlaceholders) p

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRow :: Type -> String
prettyPrintRow t = (\(Tuple tys rest) -> joinWith ", " (map (uncurry nameAndTypeToPs) tys) ++ tailToPs rest) $ toList [] t
  where
  nameAndTypeToPs :: String -> Type -> String
  nameAndTypeToPs name ty = prettyPrintObjectKey name ++ " :: " ++ prettyPrintType ty
  tailToPs :: Type -> String
  tailToPs REmpty = ""
  tailToPs other = " | " ++ prettyPrintType other
  toList :: [Tuple String Type] -> Type -> (Tuple [Tuple String Type] Type)
  toList tys (RCons name ty row) = toList ((Tuple name ty):tys) row
  toList tys r = (Tuple tys r)
