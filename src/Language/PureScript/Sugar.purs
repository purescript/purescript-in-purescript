-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Desugaring passes
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar (desugar) where

import Data.Either
import Data.Traversable (traverse)

import Control.Bind ((>=>))
import Control.Monad
import Control.Monad.Trans

import Language.PureScript.Declarations
import Language.PureScript.Errors
import Language.PureScript.Supply

{-
import Language.PureScript.Sugar.Operators
import Language.PureScript.Sugar.TypeClasses
import Language.PureScript.Sugar.Names
-}

import Language.PureScript.Sugar.CaseDeclarations
import Language.PureScript.Sugar.BindingGroups
import Language.PureScript.Sugar.DoNotation
import Language.PureScript.Sugar.TypeDeclarations

-- |
-- The desugaring pipeline proceeds as follows:
--
--  * Introduce type synonyms for type class dictionaries
--
--  * Rebracket user-defined binary operators
--
--  * Desugar do-notation using the @Prelude.Monad@ type class
--
--  * Desugar top-level case declarations into explicit case expressions
--
--  * Desugar type declarations into value declarations with explicit type annotations
--
--  * Group mutually recursive value and data declarations into binding groups.
--
--  * Qualify any unqualified names and types
--
desugar :: [Module] -> SupplyT (Either ErrorStack) [Module]
desugar = {- map removeSignedLiterals
          >>> -}
          traverse desugarDoModule
          >=> desugarCasesModule
          >=> lift <<< (desugarTypeDeclarationsModule
                      {- >=> desugarImports
                      >=> rebracket -})
          {- >=> desugarTypeClasses 
          -}
          >=>  lift <<< createBindingGroupsModule