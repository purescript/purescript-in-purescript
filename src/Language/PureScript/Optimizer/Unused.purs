module Language.PureScript.Optimizer.Unused
  ( removeUnusedVariables
  , removeCodeAfterReturnStatements
  ) where

import Data.Array (span)
import Data.Foldable (any)
import Data.Tuple
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.Optimizer.Common

removeUnusedVariables :: JS -> JS
removeUnusedVariables = everywhereOnJS (removeFromBlock withBlock)
  where
  withBlock :: [JS] -> [JS]
  withBlock sts = go sts sts
  go :: [JS] -> [JS] -> [JS]
  go _ [] = []
  go sts (JSVariableIntroduction var _ : rest) | not (any (isUsed var) sts) = go sts rest
  go sts (s : rest) = s : go sts rest

removeCodeAfterReturnStatements :: JS -> JS
removeCodeAfterReturnStatements = everywhereOnJS (removeFromBlock go)
  where
  go :: [JS] -> [JS]
  go jss | not (any isJSReturn jss) = jss
  go jss = case span (not <<< isJSReturn) jss of
    { init = body, rest = (ret : _) } -> body ++ [ret]
  isJSReturn (JSReturn _) = true
  isJSReturn _ = false
