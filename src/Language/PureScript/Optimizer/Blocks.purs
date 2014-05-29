module Language.PureScript.Optimizer.Blocks (collapseNestedBlocks) where

import Data.Array (concatMap)
import Language.PureScript.CodeGen.JS.AST

-- |
-- Collapse blocks which appear nested directly below another block
--
collapseNestedBlocks :: JS -> JS
collapseNestedBlocks = everywhereOnJS collapse
  where
  collapse :: JS -> JS
  collapse (JSBlock sts) = JSBlock (concatMap go sts)
  collapse js = js
  go :: JS -> [JS]
  go (JSBlock sts) = sts
  go s = [s]
