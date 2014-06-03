module Interactive where

import Data.Tuple
import Data.Either
import Data.String (joinWith, indexOf, drop)

import Debug.Trace

import Control.Apply
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Process
import Control.Monad.Eff.FS

import Node.Args
import Node.ReadLine

import Text.Parsing.Parser

import Language.PureScript.Names
import Language.PureScript.Declarations (Value())

import qualified Language.PureScript.Parser.Lexer as P
import qualified Language.PureScript.Parser.Common as P
import qualified Language.PureScript.Parser.Declarations as P

-- |
-- Valid Meta-commands for PSCI
--
data Command
  -- |
  -- A purescript expression
  --
  = Eval Value
  -- |
  -- Show the help command
  --
  | Help
  -- |
  -- Import a module from a loaded file
  --
  | Import ModuleName
  -- |
  -- Load a file for use with importing
  --
  | LoadFile String
  -- |
  -- Exit PSCI
  --
  | Quit
  -- |
  -- Reset the state of the REPL
  --
  | Reset
  -- |
  -- Binds a value to a name
  --
  | Let (Value -> Value)
  -- |
  -- Find the type of an expression
  --
  | TypeOf Value
  
parse :: forall a. Parser P.TokenStream a -> String -> Either String a
parse p s = P.lex s >>= P.runTokenParser (p <* P.eof)
  
parseCommand :: String -> Either String Command
parseCommand ":?" = Right Help
parseCommand ":q" = Right Quit
parseCommand ":r" = Right Reset
parseCommand cmd | indexOf ":i " cmd == 0 = Import <$> parse P.moduleName (drop 3 cmd)
parseCommand cmd | indexOf ":m " cmd == 0 = Right $ LoadFile (drop 3 cmd)
parseCommand cmd | indexOf ":t " cmd == 0 = TypeOf <$> parse (P.parseValue {}) (drop 3 cmd)
parseCommand cmd | indexOf ":" cmd == 0 = Left "Unknown command. Type :? for help."

-- |
-- The help menu.
--
help :: String
help = 
  "  :?            Show this help menu\n\
  \  :i <module>   Import <module> for use in PSCi\n\
  \  :m <file>     Load <file> for importing\n\
  \  :q            Quit PSCi\n\
  \  :r            Reset\n\
  \  :t <expr>     Show the type of <expr>"

prologueMessage :: String
prologueMessage = 
  " ____                 ____            _       _   \n\
  \|  _ \\ _   _ _ __ ___/ ___|  ___ _ __(_)_ __ | |_ \n\
  \| |_) | | | | '__/ _ \\___ \\ / __| '__| | '_ \\| __|\n\
  \|  __/| |_| | | |  __/___) | (__| |  | | |_) | |_ \n\
  \|_|    \\__,_|_|  \\___|____/ \\___|_|  |_| .__/ \\__|\n\
  \                                       |_|        \n\
  \\n\
  \:? shows help\n\
  \\n\
  \Expressions are terminated using Ctrl+D"
  
completion :: forall eff. Completer eff
completion s = return $ Tuple [] s

handleCommand :: Command -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console) {}
handleCommand Help = trace help
handleCommand Quit = trace "See ya!" *> exit 0
handleCommand cmd = return {}

lineHandler :: String -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console) {}
lineHandler input = 
  case parseCommand input of
    Left msg -> trace msg
    Right cmd -> handleCommand cmd

loop :: [String] -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console) {}
loop inputFiles = do
  interface <- createInterface process.stdin process.stdout completion
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler (\s -> lineHandler s <* prompt interface) interface
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
