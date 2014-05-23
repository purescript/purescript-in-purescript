module Language.PureScript.TypeChecker.Synonyms (saturateAllTypeSynonyms) where

import Language.PureScript.Types
import Language.PureScript.Names

import Data.Either
import Data.Maybe
import Data.Tuple
import Control.Monad (foldM)
import Control.Monad.Writer
import Control.Monad.Error
import Control.Monad.Error.Class

-- |
-- Build a type substitution for a type synonym
--
buildTypeSubstitution :: Qualified ProperName -> Number -> Type -> Either String (Maybe Type)
buildTypeSubstitution name n = go n []
  where
  go :: Number -> [Type] -> Type -> Either String (Maybe Type)
  go 0 args (TypeConstructor ctor) | name == ctor = return (Just $ SaturatedTypeSynonym ctor args)
  go m _ (TypeConstructor ctor) | m > 0 && name == ctor = throwError $ "Partially applied type synonym " ++ show name
  go m args (TypeApp f arg) = go (m - 1) (arg:args) f
  go _ _ _ = return Nothing

-- |
-- Replace all instances of a specific type synonym with the @SaturatedTypeSynonym@ data constructor
--
saturateTypeSynonym :: Qualified ProperName -> Number -> Type -> Either String Type
saturateTypeSynonym name n = everywhereOnTypesTopDownM replace
  where
  replace t = fromMaybe t <$> buildTypeSubstitution name n t

-- |
-- Replace all type synonyms with the @SaturatedTypeSynonym@ data constructor
--
saturateAllTypeSynonyms :: [Tuple (Qualified ProperName) Number] -> Type -> Either String Type
saturateAllTypeSynonyms syns d = foldM (\result (Tuple name n) -> saturateTypeSynonym name n result) d syns
