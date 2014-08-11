module Control.Monad.Eff.FS where

import Data.Date
import Data.Either

import Control.Monad.Eff
import Control.Monad.Eff.Exception

import qualified Control.Monad.Eff.Unsafe as U

import Node.Encoding
import Node.FS
import Node.FS.Stats
import Node.FS.Sync (readTextFile, writeTextFile, stat)
import Node.Path

liftFSAction :: forall eff r. Eff (fs :: FS, err :: Exception | eff) r -> Eff (fs :: FS | eff) (Either Error r)
liftFSAction fs = U.unsafeInterleaveEff (catchException (return <<< Left) $ Right <$> fs)

readFile :: forall eff r. FilePath -> (String -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
readFile path f g = either g f <$> (liftFSAction $ readTextFile UTF8 path)

writeFile :: forall eff r. FilePath -> String -> (Unit -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
writeFile path content f g = either g f <$> (liftFSAction $ writeTextFile UTF8 path content)

doesFileExist :: forall eff r. FilePath -> (Boolean -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
doesFileExist path f g = either (f <<< const false) (f <<< const true) <$> (liftFSAction $ stat path)

getModificationTime :: forall eff r. FilePath -> (Number -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r
getModificationTime path f g = either g (f <<< toEpochMilliseconds <<< modifiedTime) <$> (liftFSAction $ stat path)

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
