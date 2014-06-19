module Control.Monad.Application where

import Data.Either
import Data.Maybe
import Debug.Trace
import Control.Monad.Trans
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Trans
import Control.Monad.Eff
import Control.Monad.Eff.FS
import Control.Monad.Eff.Process
import Global
import Language.PureScript
import Node.FS
import Node.Path

data Application a = Application (forall eff. ErrorT String (Eff (fs :: FS, trace :: Trace, process :: Process | eff)) a)

unApplication :: forall eff a. Application a -> ErrorT String (Eff (fs :: FS, trace :: Trace, process :: Process | eff)) a
unApplication (Application m) = m

instance functorApplication :: Functor Application where
  (<$>) f (Application m) = Application (f <$> m)

instance applyApplication :: Apply Application where
  (<*>) (Application f) (Application x) = Application (f <*> x)

instance applicativeApplication :: Applicative Application where
  pure a = Application (pure a)

instance bindApplication :: Bind Application where
  (>>=) (Application m) f = Application (m >>= (unApplication <<< f))

instance monadApplication :: Monad Application

instance monadErrorApplication :: MonadError String Application where
  throwError e = Application (throwError e)
  catchError (Application e) f = Application (catchError e (unApplication <<< f))

instance monadMakeApp :: MonadMake Application where
  getTimestamp path = do
    exists <- doesFileExistApplication path
    case exists of
      true -> Just <$> getModificationTimeApplication path
      false -> return Nothing
  readTextFile path = do
    effApplication (trace $ "Reading " ++ path)
    readFileApplication path
  writeTextFile path text = do
    mkdirpApplication (dirname path)
    effApplication (trace $ "Writing " ++ path)
    writeFileApplication path text
  liftError = eitherApplication
  progress msg = effApplication (trace msg)

runApplication :: forall eff a. Application a -> Eff (fs :: FS, trace :: Trace, process :: Process | eff) Unit
runApplication (Application app) = do
  result <- runErrorT app
  case result of
    Left err -> do
      trace err
      exit 1
    Right _ -> exit 0

runApplication' :: forall eff a. Application a -> Eff (fs :: FS, trace :: Trace, process :: Process | eff) (Either String a)
runApplication' (Application app) = runErrorT app

fsAction :: forall a. (forall eff r. (a -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r) -> Application a
fsAction k = Application (ErrorT (k Right (Left <<< show)))

readFileApplication :: String -> Application String
readFileApplication filename = fsAction (readFile filename)

writeFileApplication :: String -> String -> Application Unit
writeFileApplication filename text = fsAction (writeFile filename text)

doesFileExistApplication :: String -> Application Boolean
doesFileExistApplication filename = fsAction (doesFileExist filename)

getModificationTimeApplication :: String -> Application Number
getModificationTimeApplication filename = fsAction (getModificationTime filename)

mkdirpApplication :: String -> Application Unit
mkdirpApplication filename = fsAction (mkdirp filename)

eitherApplication :: forall a. Either String a -> Application a
eitherApplication e = Application (ErrorT (return e))

effApplication :: forall a. (forall eff. Eff (fs :: FS, trace :: Trace, process :: Process | eff) a) -> Application a
effApplication a = Application (lift a)
