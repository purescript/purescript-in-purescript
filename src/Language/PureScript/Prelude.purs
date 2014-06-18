module Language.PureScript.Prelude where

import Language.PureScript (procFilePath)
import Node.Path

preludeBaseDir :: String
preludeBaseDir = join (dirname procFilePath) "../prelude/"

preludeFiles :: [String]
preludeFiles =
  [ preludeBaseDir ++ "Prelude.purs"
  , preludeBaseDir ++ "Prelude/Unsafe.purs"
  , preludeBaseDir ++ "Control/Monad/Eff.purs"
  , preludeBaseDir ++ "Control/Monad/Eff/Unsafe.purs"
  , preludeBaseDir ++ "Control/Monad/ST.purs"
  , preludeBaseDir ++ "Data/Eq.purs"
  , preludeBaseDir ++ "Data/Function.purs"
  , preludeBaseDir ++ "Debug/Trace.purs"
  ]
