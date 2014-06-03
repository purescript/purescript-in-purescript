module Control.Monad.Application where
  
import Data.Either  
  
import Debug.Trace  

import Control.Monad.Trans
  
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Trans

import Control.Monad.Eff
import Control.Monad.Eff.FS
import Control.Monad.Eff.Process

data Application a = Application (ErrorT String (Eff (fs :: FS, trace :: Trace, process :: Process)) a)

unApplication :: forall a. Application a -> ErrorT String (Eff (fs :: FS, trace :: Trace, process :: Process)) a
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
  catchError (Application e) f = Application $ catchError e (unApplication <<< f)

runApplication :: forall a. Application a -> Eff (fs :: FS, trace :: Trace, process :: Process) {}
runApplication (Application app) = do
  result <- runErrorT app
  case result of
    Left err -> do
      trace err
      exit 1
    Right _ -> exit 0
    
fsAction :: forall a. (forall eff r. (a -> r) -> (FSError -> r) -> Eff (fs :: FS | eff) r) -> Application a
fsAction k = Application $ ErrorT $ k Right (Left <<< getStackTrace)

readFileApplication :: String -> Application String
readFileApplication filename = fsAction (readFile filename)

writeFileApplication :: String -> String -> Application {}
writeFileApplication filename text = fsAction (writeFile filename text)

doesFileExistApplication :: String -> Application Boolean
doesFileExistApplication filename = fsAction (doesFileExist filename)

getModificationTimeApplication :: String -> Application Number
getModificationTimeApplication filename = fsAction (getModificationTime filename)

mkdirpApplication :: String -> Application {}
mkdirpApplication filename = fsAction (mkdirp filename)

eitherApplication :: forall a. Either String a -> Application a
eitherApplication e = Application $ ErrorT (return e)

effApplication :: forall a. Eff (fs :: FS, trace :: Trace, process :: Process) a -> Application a
effApplication a = Application $ lift a