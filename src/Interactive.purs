module Interactive where

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Array (map)
import Data.Foldable (foldl)
import Data.String (joinWith, indexOf, drop)

import Debug.Trace

import Control.Apply
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Process
import Control.Monad.Eff.FS

import Node.Args
import Node.ReadLine

import Language.PureScript.Names
import Language.PureScript.Environment (NameKind(..))
import qualified Language.PureScript.Declarations as D

import qualified Text.Parsing.Parser as P
import qualified Text.Parsing.Parser.Combinators as P
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
  = Eval D.Value
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
  | Let (D.Value -> D.Value)
  -- |
  -- Find the type of an expression
  --
  | TypeOf D.Value
  
parse :: forall a. P.Parser P.TokenStream a -> String -> Either String a
parse p s = P.lex s >>= P.runTokenParser (p <* P.eof)

parseLet :: P.Parser P.TokenStream Command
parseLet = Let <$> (D.Let <$> (P.reserved "let" *> P.braces (P.many1 P.parseDeclaration)))
  
parseCommand :: String -> Either String Command
parseCommand ":?" = Right Help
parseCommand ":q" = Right Quit
parseCommand ":r" = Right Reset
parseCommand cmd | indexOf ":i " cmd == 0 = Import <$> parse P.moduleName (drop 3 cmd)
parseCommand cmd | indexOf ":m " cmd == 0 = Right $ LoadFile (drop 3 cmd)
parseCommand cmd | indexOf ":t " cmd == 0 = TypeOf <$> parse (P.parseValue {}) (drop 3 cmd)
parseCommand cmd | indexOf ":" cmd == 0 = Left "Unknown command. Type :? for help."
parseCommand cmd = parse (parseLet <|> (Eval <$> P.parseValue {})) cmd
  
-- |
-- The PSCI state.
-- Holds a list of imported modules, loaded files, and partial let bindings.
-- The let bindings are partial,
-- because it makes more sense to apply the binding to the final evaluated expression.
--
type PSCIState = { importedFilenames   :: [String]
                 , importedModuleNames :: [ModuleName]
                 , loadedModules       :: [Tuple String D.Module]
                 , letBindings         :: [D.Value -> D.Value]
                 }
  
emptyPSCIState :: PSCIState
emptyPSCIState = { importedFilenames   : []
                 , importedModuleNames : []
                 , loadedModules       : []
                 , letBindings         : []
                 }
                 
defaultImports :: [ModuleName]
defaultImports = [ModuleName [ProperName "Prelude"]]

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
 
moduleFromText :: String -> Either String D.Module
moduleFromText text = do
  tokens <- P.lex text
  P.runTokenParser P.parseModule tokens
  
-- |
-- Load a module from a file
--
loadModule :: forall eff. String -> Eff (fs :: FS | eff) (Either String D.Module)
loadModule filename = readFile filename moduleFromText (Left <<< getStackTrace)

-- |
-- Makes a temporary module for the purposes of executing an expression
--
createTemporaryModule :: Boolean -> PSCIState -> D.Value -> D.Module
createTemporaryModule exec st value =
  let
    moduleName :: ModuleName
    moduleName = ModuleName [ProperName "Main"]
    
    traceModule :: ModuleName
    traceModule = ModuleName [ProperName "Debug", ProperName "Trace"]
    
    trace :: D.Value
    trace = D.Var (Qualified (Just traceModule) (Ident "print"))
    
    itValue :: D.Value
    itValue = foldl (\x f -> f x) value st.letBindings
    
    mainValue :: D.Value
    mainValue = D.App trace (D.Var (Qualified Nothing (Ident "it")))
    
    importDecl :: ModuleName -> D.Declaration
    importDecl m = D.ImportDeclaration m Nothing Nothing
    
    itDecl :: D.Declaration
    itDecl = D.ValueDeclaration (Ident "it") Value [] Nothing itValue
    
    mainDecl :: D.Declaration
    mainDecl = D.ValueDeclaration (Ident "main") Value [] Nothing mainValue
    
    decls :: [D.Declaration]
    decls = if exec then [itDecl, mainDecl] else [itDecl]
  in
    D.Module moduleName (map importDecl st.importedModuleNames ++ decls) Nothing

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
  
completion :: forall eff. RefVal PSCIState -> Completer (ref :: Ref | eff)
completion state s = return $ Tuple [] s

handleCommand :: Command -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref) {}
handleCommand Help = trace help
handleCommand Quit = trace "See ya!" *> exit 0
handleCommand cmd = return {}

lineHandler :: RefVal PSCIState -> String -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref) {}
lineHandler state input = 
  case parseCommand input of
    Left msg -> trace msg
    Right cmd -> handleCommand cmd

loop :: RefVal PSCIState -> [String] -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref) {}
loop state inputFiles = do
  interface <- createInterface process.stdin process.stdout (completion state)
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler (\s -> lineHandler state s <* prompt interface) interface
  return {}
  
inputFiles :: Args [String]
inputFiles = many argOnly  
  
term :: RefVal PSCIState -> Args (Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref) {})
term state = loop state <$> inputFiles

main = do
  state <- newRef emptyPSCIState
  trace prologueMessage
  result <- readArgs' (term state)
  case result of
    Left err -> print err
    _ -> return {}
