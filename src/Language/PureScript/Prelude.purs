module Language.PureScript.Prelude where

import Node.Path

foreign import procFile
  "var procFile = require('fs').realpathSync(process.argv[1]);" :: String

preludeBaseDir :: String
preludeBaseDir = join (dirname procFile) "../prelude/"

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
