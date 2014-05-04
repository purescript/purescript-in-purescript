module Language.PureScript.CodeGen.Common where

import Data.Array (concatMap, elem, map)
import Data.Foldable (all, foldMap)
import Data.Monoid ()
import Data.String (charCodeAt, joinWith, split)
import Data.String.Regex (Regex(..), regex, test)
import Language.PureScript.Names

-- |
-- Convert an Ident into a valid Javascript identifier:
--
--  * Alphanumeric characters are kept unmodified.
--
--  * Reserved javascript identifiers are prefixed with '$$'.
--
--  * Symbols are prefixed with '$' followed by a symbol name or their ordinal value.
--
identToJs :: Ident -> String
identToJs (Ident name) | nameIsJsReserved name = "$$" ++ name
identToJs (Ident name) = foldMap identCharToString (split name "")
identToJs (Op op) = foldMap identCharToString (split op "")

-- |
-- Attempts to find a human-readable name for a symbol, if none has been specified returns the
-- ordinal value.
--
identCharToString :: String -> String
identCharToString c | test rxAlphaNum c = c
identCharToString "_" = "_"
identCharToString "." = "$dot"
identCharToString "$" = "$dollar"
identCharToString "~" = "$tilde"
identCharToString "=" = "$eq"
identCharToString "<" = "$less"
identCharToString ">" = "$greater"
identCharToString "!" = "$bang"
identCharToString "#" = "$hash"
identCharToString "%" = "$percent"
identCharToString "^" = "$up"
identCharToString "&" = "$amp"
identCharToString "|" = "$bar"
identCharToString "*" = "$times"
identCharToString "/" = "$div"
identCharToString "+" = "$plus"
identCharToString "-" = "$minus"
identCharToString ":" = "$colon"
identCharToString "\\" = "$bslash"
identCharToString "?" = "$qmark"
identCharToString "@" = "$at"
identCharToString "\'" = "$prime"
identCharToString c = "$" ++ show (charCodeAt 0 c)

-- |
-- Checks whether an identifier name is reserved in Javascript.
--
nameIsJsReserved :: String -> Boolean
nameIsJsReserved name =
  name `elem` [ "abstract"
              , "arguments"
              , "boolean"
              , "break"
              , "byte"
              , "case"
              , "catch"
              , "char"
              , "class"
              , "const"
              , "continue"
              , "debugger"
              , "default"
              , "delete"
              , "do"
              , "double"
              , "else"
              , "enum"
              , "eval"
              , "export"
              , "extends"
              , "final"
              , "finally"
              , "float"
              , "for"
              , "function"
              , "goto"
              , "if"
              , "implements"
              , "import"
              , "in"
              , "instanceof"
              , "int"
              , "interface"
              , "let"
              , "long"
              , "native"
              , "new"
              , "null"
              , "package"
              , "private"
              , "protected"
              , "public"
              , "return"
              , "short"
              , "static"
              , "super"
              , "switch"
              , "synchronized"
              , "this"
              , "throw"
              , "throws"
              , "transient"
              , "try"
              , "typeof"
              , "var"
              , "void"
              , "volatile"
              , "while"
              , "with"
              , "yield" ]

-- |
-- Test if a string is a valid JS identifier (may return false negatives)
--
isIdent :: String -> Boolean
isIdent = test rxIdent

moduleNameToJs :: ModuleName -> String
moduleNameToJs (ModuleName pns) = joinWith "_" (runProperName `map` pns)

rxAlphaNum :: Regex
rxAlphaNum = regex "[a-z0-9]" "i"

rxIdent :: Regex
rxIdent = regex "^[a-z][a-z0-9]+$" "i"
