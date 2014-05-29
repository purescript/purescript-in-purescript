module Language.PureScript.Optimizer (optimize) where

import Data.Tuple
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Options
import Language.PureScript.Optimizer.Common
import Language.PureScript.Optimizer.TCO
import Language.PureScript.Optimizer.MagicDo
import Language.PureScript.Optimizer.Inliner
import Language.PureScript.Optimizer.Unused
import Language.PureScript.Optimizer.Blocks
import qualified Language.PureScript.Constants as C

-- |
-- Apply a series of optimizer passes to simplified Javascript code
--
optimize :: Options -> JS -> JS
optimize (Options o) | o.noOptimizations = id
optimize opts = untilFixedPoint (applyAll
  [ collapseNestedBlocks
  , tco opts
  , magicDo opts
  , removeUnusedVariables
  , removeCodeAfterReturnStatements
  , unThunk
  , etaConvert
  , evaluateIifes
  , inlineVariables
  -- , inlineOperator (C.prelude, (C.$)) $ \f x -> JSApp f [x]
  -- , inlineOperator (C.prelude, (C.#)) $ \x f -> JSApp f [x]
  , inlineOperator (Tuple C.preludeUnsafe C.unsafeIndex) $ flip JSIndexer
  , inlineCommonOperators ])

untilFixedPoint :: forall a. (Eq a) => (a -> a) -> a -> a
untilFixedPoint f = go
  where
  go a = let a' = f a in
          if a' == a then a' else go a'
