module Interactive where

import Data.Tuple
import Data.Either
import Data.String (joinWith)

import Debug.Trace

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Process
import Control.Monad.Eff.FS

import Node.Args
import Node.ReadLine

completion :: forall eff. Completer eff
completion s = return $ Tuple [] s

lineHandler :: String -> Eff (fs :: FS, trace :: Trace, process :: Process) {}
lineHandler s = trace $ "You typed: " ++ s

prologueMessage :: String
prologueMessage = joinWith "\n"
  [ " ____                 ____            _       _   "
  , "|  _ \\ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ "
  , "| |_) | | | | '__/ _ \\___ \\ / __| '__| | '_ \\| __|"
  , "|  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ "
  , "|_|    \\__,_|_|  \\___|____/ \\___|_|  |_| .__/ \\__|"
  , "                                       |_|        "
  , ""
  , ":? shows help"
  , ""
  , "Expressions are terminated using Ctrl+D"
  ]

loop :: [String] -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console) {}
loop inputFiles = do
  interface <- createInterface process.stdin process.stdout completion
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler lineHandler interface
  return {}
  
inputFiles :: Args [String]
inputFiles = many argOnly  
  
term :: Args (Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console) {})
term = loop <$> inputFiles

main = do
  trace prologueMessage
  result <- readArgs' term
  case result of
    Left err -> print err
    _ -> return {}
