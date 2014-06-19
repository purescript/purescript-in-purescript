module Control.Monad.Eff.FS where

import Control.Monad.Eff
import Data.Date
import Data.Either
import Global
import Node.Encoding
import Node.FS
import Node.FS.Stats
import Node.FS.Sync (readTextFile, writeTextFile, stat)
import Node.Path

readFile :: forall eff r. FilePath -> (String -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
readFile path f g = either g f <$> readTextFile UTF8 path

writeFile :: forall eff r. FilePath -> String -> (Unit -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
writeFile path content f g = either g f <$> writeTextFile UTF8 path content

doesFileExist :: forall eff r. FilePath -> (Boolean -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
doesFileExist path f g = either (f <<< const false) (f <<< const true) <$> stat path

getModificationTime :: forall eff r. FilePath -> (Number -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
getModificationTime path f g = either g (f <<< toEpochMilliseconds <<< modifiedTime) <$> stat path

foreign import mkdirp
  "function mkdirp(filename) {\
  \  return function(k) {\
  \    return function(fail) {\
  \      return function() {\
  \        try {\
  \          return k(require('mkdirp').sync(filename));\
  \        } catch(err) {\
  \          return fail(err);\
  \        }\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. FilePath -> (Unit -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
