module Language.PureScript.Optimizer.TCO (tco) where

import Data.Array (reverse, concat, map, zipWith)
import Data.Maybe
import Data.Tuple3
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST

-- |
-- Eliminate tail calls
--
tco :: Options -> JS -> JS
tco (Options o) | o.noTco = id
tco _ = tco'

tco' :: JS -> JS
tco' = everywhereOnJS convert
  where
  tcoLabel :: String
  tcoLabel = "tco"
  tcoVar :: String -> String
  tcoVar arg = "__tco_" ++ arg
  copyVar :: String -> String
  copyVar arg = "__copy_" ++ arg
  convert :: JS -> JS
  convert js@(JSVariableIntroduction name (Just fn@(JSFunction _ _ _))) =
    case collectAllFunctionArgs [] id fn of
      Tuple3 argss body' replace | isTailCall name body' ->
        let allArgs = reverse $ concat argss
        in JSVariableIntroduction name (Just (replace (toLoop name allArgs body')))
      _ -> js
  convert js = js
  collectAllFunctionArgs :: [[String]] -> (JS -> JS) -> JS -> (Tuple3 [[String]] JS (JS -> JS))
  collectAllFunctionArgs allArgs f (JSFunction ident args (JSBlock ((body@(JSReturn _)):_))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSFunction ident (map copyVar args) (JSBlock [b]))) body
  collectAllFunctionArgs allArgs f (JSFunction ident args body@(JSBlock _)) =
    Tuple3 (args : allArgs) body (f <<< JSFunction ident (map copyVar args))
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args (JSBlock [body]))) =
    collectAllFunctionArgs (args : allArgs) (\b -> f (JSReturn (JSFunction ident (map copyVar args) (JSBlock [b])))) body
  collectAllFunctionArgs allArgs f (JSReturn (JSFunction ident args body@(JSBlock _))) =
    Tuple3 (args : allArgs) body (f <<< JSReturn <<< JSFunction ident (map copyVar args))
  collectAllFunctionArgs allArgs f body = Tuple3 allArgs body f
  isTailCall :: String -> JS -> Boolean
  isTailCall ident js =
    let
      numSelfCalls = everythingOnJS (+) countSelfCalls js
      numSelfCallsInTailPosition = everythingOnJS (+) countSelfCallsInTailPosition js
      numSelfCallsUnderFunctions = everythingOnJS (+) countSelfCallsUnderFunctions js
    in
      numSelfCalls > 0
      && numSelfCalls == numSelfCallsInTailPosition
      && numSelfCallsUnderFunctions == 0
    where
    countSelfCalls :: JS -> Number
    countSelfCalls (JSApp (JSVar ident') _) | ident == ident' = 1
    countSelfCalls _ = 0
    countSelfCallsInTailPosition :: JS -> Number
    countSelfCallsInTailPosition (JSReturn ret) | isSelfCall ident ret = 1
    countSelfCallsInTailPosition _ = 0
    countSelfCallsUnderFunctions (JSFunction _ _ js') = everythingOnJS (+) countSelfCalls js'
    countSelfCallsUnderFunctions _ = 0
  toLoop :: String -> [String] -> JS -> JS
  toLoop ident allArgs js = JSBlock $
        map (\arg -> JSVariableIntroduction arg (Just (JSVar (copyVar arg)))) allArgs ++
        [ JSLabel tcoLabel $ JSWhile (JSBooleanLiteral true) (JSBlock [ everywhereOnJS loopify js ]) ]
    where
    loopify :: JS -> JS
    loopify (JSReturn ret) | isSelfCall ident ret =
      let
        allArgumentValues = concat $ collectSelfCallArgs [] ret
      in
        JSBlock $ zipWith (\val arg ->
                    JSVariableIntroduction (tcoVar arg) (Just val)) allArgumentValues allArgs
                  ++ map (\arg ->
                    JSAssignment (JSVar arg) (JSVar (tcoVar arg))) allArgs
                  ++ [ JSContinue tcoLabel ]
    loopify other = other
    collectSelfCallArgs :: [[JS]] -> JS -> [[JS]]
    collectSelfCallArgs allArgumentValues (JSApp fn args') = collectSelfCallArgs (args' : allArgumentValues) fn
    collectSelfCallArgs allArgumentValues _ = allArgumentValues
  isSelfCall :: String -> JS -> Boolean
  isSelfCall ident (JSApp (JSVar ident') _) | ident == ident' = true
  isSelfCall ident (JSApp fn _) = isSelfCall ident fn
  isSelfCall _ _ = false
