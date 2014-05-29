module Language.PureScript.Optimizer.Common where

import Data.Foldable (foldl, lookup, elem, any)
import Data.Maybe (fromMaybe)
import Data.Tuple
import Language.PureScript.Errors
import Language.PureScript.CodeGen.JS.AST

applyAll :: forall a. [a -> a] -> a -> a
applyAll (f : fs) = foldl (<<<) f fs

replaceIdent :: String -> JS -> JS -> JS
replaceIdent var1 js = everywhereOnJS replace
  where
  replace (JSVar var2) | var1 == var2 = js
  replace other = other

replaceIdents :: [Tuple String JS] -> JS -> JS
replaceIdents vars = everywhereOnJS replace
  where
  replace v@(JSVar var) = fromMaybe v $ lookup var vars
  replace other = other

isReassigned :: String -> JS -> Boolean
isReassigned var1 = everythingOnJS (||) check
  where
  check :: JS -> Boolean
  check (JSFunction _ args _) | var1 `elem` args = true
  check (JSVariableIntroduction arg _) | var1 == arg = true
  check (JSAssignment (JSVar arg) _) | var1 == arg = true
  check (JSFor arg _ _ _) | var1 == arg = true
  check (JSForIn arg _ _) | var1 == arg = true
  check _ = false

isRebound :: JS -> JS -> Boolean
isRebound js d = any (\v -> isReassigned v d || isUpdated v d) (everythingOnJS (++) variablesOf js)
  where
  variablesOf (JSVar var) = [var]
  variablesOf _ = []

isUsed :: String -> JS -> Boolean
isUsed var1 = everythingOnJS (||) check
  where
  check :: JS -> Boolean
  check (JSVar var2) | var1 == var2 = true
  check (JSAssignment target _) | var1 == targetVariable target = true
  check _ = false

targetVariable :: JS -> String
targetVariable (JSVar var) = var
targetVariable (JSAccessor _ tgt) = targetVariable tgt
targetVariable (JSIndexer _ tgt) = targetVariable tgt
targetVariable _ = theImpossibleHappened "Invalid argument to targetVariable"

isUpdated :: String -> JS -> Boolean
isUpdated var1 = everythingOnJS (||) check
  where
  check :: JS -> Boolean
  check (JSAssignment target _) | var1 == targetVariable target = true
  check _ = false

removeFromBlock :: ([JS] -> [JS]) -> JS -> JS
removeFromBlock go (JSBlock sts) = JSBlock (go sts)
removeFromBlock _  js = js
