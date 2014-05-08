module Language.PureScript.Pretty.Common where

import Control.Monad.State.Trans
import Control.Monad.State.Class
import Data.Array (map)
import Data.Foldable (elem)
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)
import Data.String (charAt, joinWith)

import Language.PureScript.Errors (theImpossibleHappened)
import Language.PureScript.Keywords

-- |
-- Wrap a string in parentheses
--
parens :: String -> String
parens s = "(" ++ s ++ ")"

data PrinterState = PrinterState { indent :: Number }

instance showPrinterState :: Show PrinterState where
  show (PrinterState { indent = i }) = "PrinterState { indent: " ++ show i ++ " }"
  
instance eqPrinterState :: Eq PrinterState where
  (==) (PrinterState { indent = i1 }) (PrinterState { indent = i2 }) = i1 == i2
  (/=) x y = not (x == y)
  
instance ordPrinterState :: Ord PrinterState where
  compare (PrinterState { indent = i1 }) (PrinterState { indent = i2 }) = compare i1 i2

-- |
-- Number of characters per identation level
--
blockIndent :: Number
blockIndent = 4

-- |
-- Pretty print with a new indentation level
--
withIndent :: StateT PrinterState Maybe String -> StateT PrinterState Maybe String
withIndent action = do
  modify $ \(PrinterState st) -> PrinterState $ st { indent = st.indent + blockIndent }
  result <- action
  modify $ \(PrinterState st) -> PrinterState $ st { indent = st.indent - blockIndent }
  return result

-- |
-- Get the current indentation level
--
currentIndent :: StateT PrinterState Maybe String
currentIndent = do
  PrinterState { indent = current } <- get
  return $ replicate current " "
  where
  replicate 0 x = ""
  replicate n x = x ++ replicate (n - 1) x

-- |
-- Print many lines
--
prettyPrintMany :: forall a. (a -> StateT PrinterState Maybe String) -> [a] -> StateT PrinterState Maybe String
prettyPrintMany f xs = do
  ss <- traverse f xs
  indentString <- currentIndent
  return $ joinWith "\n" $ map ((++) indentString) ss

-- |
-- Prints an object key, escaping reserved names.
--
prettyPrintObjectKey :: String -> String
prettyPrintObjectKey s | s `elem` reservedPsNames = show s
prettyPrintObjectKey s | charAt 0 s `elem` opChars = show s
prettyPrintObjectKey s = s

runPretty :: forall a b. (a -> Maybe b) -> a -> b
runPretty p x = case p x of
  Just x -> x
  Nothing -> theImpossibleHappened "Incomplete pattern"