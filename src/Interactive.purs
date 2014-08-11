module Interactive where

import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Array (map, nub, mapMaybe, filter, sort, last)
import Data.Foldable (foldl, any)
import Data.Traversable (for, traverse, sequence)
import Data.String (joinWith, indexOf, drop, length)
import Data.String.Regex (regex, match, parseFlags)

import qualified Data.Map as M

import Debug.Trace

import Control.Alt
import Control.Alternative
import Control.Apply
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Process
import Control.Monad.Eff.FS (readFile)
import Control.Monad.Application
import Control.Monad.Error.Class

import Node.Args
import Node.FS
import Node.ReadLine
import Node.Path (dirname, join)

import Language.PureScript
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Prelude
import Language.PureScript.Environment
import Language.PureScript.Pretty.Types (prettyPrintType)
import Language.PureScript.CodeGen.JS (RequirePathType(..))
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
parseLet = Let <$> (D.Let <$> (P.reserved "let" *> P.braces (some P.parseDeclaration)))

parseCommand :: String -> Either String Command
parseCommand ":?" = Right Help
parseCommand ":q" = Right Quit
parseCommand ":r" = Right Reset
parseCommand cmd | indexOf ":i " cmd == 0 = Import <$> parse P.moduleName (drop 3 cmd)
parseCommand cmd | indexOf ":m " cmd == 0 = Right $ LoadFile (drop 3 cmd)
parseCommand cmd | indexOf ":t " cmd == 0 = TypeOf <$> parse (P.parseValue unit) (drop 3 cmd)
parseCommand cmd | indexOf ":" cmd == 0 = Left "Unknown command. Type :? for help."
parseCommand cmd = parse (parseLet <|> (Eval <$> P.parseValue unit)) cmd

-- |
-- The PSCI state.
-- Holds a list of imported modules, loaded files, and partial let bindings.
-- The let bindings are partial,
-- because it makes more sense to apply the binding to the final evaluated expression.
--
type PSCIState = { loadedModules       :: [Tuple String D.Module]
                 , importedModuleNames :: [ModuleName]
                 , letBindings         :: [D.Value -> D.Value]
                 }

emptyPSCIState :: [Tuple String D.Module] -> PSCIState
emptyPSCIState files = { loadedModules       : files
                       , importedModuleNames : [ModuleName [ProperName "Prelude"]]
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
loadModule filename = readFile filename moduleFromText (Left <<< show)

loadModules :: [String] -> Application [Tuple String D.Module]
loadModules input =
  for input (\inputFile -> do
    text <- readFileApplication inputFile
    case moduleFromText text of
      Left err -> throwError err
      Right m -> return (Tuple inputFile m))

-- |
-- Makes a temporary module for the purposes of executing an expression
--
createTemporaryModule :: Boolean -> PSCIState -> D.Value -> D.Module
createTemporaryModule exec st value =
  let
    moduleName :: ModuleName
    moduleName = ModuleName [ProperName "Main"]

    replModule :: ModuleName
    replModule = ModuleName [ProperName "REPL"]

    evalPrint :: D.Value
    evalPrint = D.Var (Qualified (Just replModule) (Ident "evalPrint"))

    itValue :: D.Value
    itValue = foldl (\x f -> f x) value st.letBindings

    mainValue :: D.Value
    mainValue = D.App evalPrint (D.Var (Qualified Nothing (Ident "it")))

    importDecl :: ModuleName -> D.Declaration
    importDecl m = D.ImportDeclaration m Nothing Nothing

    itDecl :: D.Declaration
    itDecl = D.ValueDeclaration (Ident "it") Value [] Nothing itValue

    mainDecl :: D.Declaration
    mainDecl = D.ValueDeclaration (Ident "main") Value [] Nothing mainValue

    decls :: [D.Declaration]
    decls = if exec then [itDecl, mainDecl] else [itDecl]

    moduleBody :: [D.Declaration]
    moduleBody = map importDecl (replModule : st.importedModuleNames) ++ decls
  in
    D.Module moduleName moduleBody Nothing

-- |
-- Require statements use absolute paths to modules cached in the current directory
--
requireMode :: RequirePathType
requireMode = RequireAbsolute modulePath

modulePath :: String -> String
modulePath mn = modulesDir ++ "/" ++ mn ++ "/index.js"

foreign import homeDirectory
  "var homeDirectory = process.env['HOME'] || process.env['HOMEPATH'] || process.env['USERPROFILE'];" :: String

-- |
-- Directory which holds compiled modules
--
modulesDir :: String
modulesDir = homeDirectory ++ "/.purescript/psci/cache"

-- |
-- The REPL support module
--
replModule :: String
replModule = join [(dirname procFilePath), "../runtime/REPL.purs"]

-- |
-- Compilation options
--
options :: Options
options = mkOptions false true false true Nothing true Nothing [] [] false

-- |
-- Takes a value and prints its type
--
handleTypeOf :: forall eff. PSCIState -> D.Value -> Eff (fs :: FS, trace :: Trace, process :: Process | eff) Unit
handleTypeOf st value = do
  let m = createTemporaryModule false st value
  e <- runApplication' do
    ms <- loadModules (map fst st.loadedModules)
    make requireMode modulesDir options (ms ++ [Tuple "Main.purs" m])
  case e of
    Left err -> trace err
    Right (Environment env') ->
      case M.lookup (Tuple (ModuleName [ProperName "Main"]) (Ident "it")) env'.names of
        Just (Tuple ty _) -> trace $ prettyPrintType ty
        Nothing -> trace "Could not find type"

-- |
-- An effect for the 'eval' function
--
foreign import data Eval :: !

-- |
-- Evaluate some Javascript
--
foreign import evaluate
  "function evaluate(js) {\
  \  return function() {\
  \    eval(js);\
  \  };\
  \}" :: forall eff. String -> Eff (eval :: Eval | eff) Unit

-- |
-- Takes a value declaration and evaluates it with the current state.
--
handleEval :: forall eff. PSCIState -> D.Value -> Eff (fs :: FS, trace :: Trace, process :: Process, eval :: Eval | eff) Unit
handleEval st value = do
  let m = createTemporaryModule true st value
  e <- runApplication' do
    ms <- loadModules (map fst st.loadedModules)
    make requireMode modulesDir options (ms ++ [Tuple "Main.purs" m])
  case e of
    Left err -> trace err
    Right _ -> evaluate $
      "(function() {\
      \  for (var k in require.cache) delete require.cache[k];\
      \  require(" ++ show (modulePath "Main") ++ ").main();\
      \})()"

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
completion state s = do
  st <- readRef state
  let ms = map snd st.loadedModules
  let suffix = fromMaybe "" $ lastIdent s
  return $ Tuple (sort (filter (isPrefixOf suffix) (names ms))) suffix
    where
    names :: [D.Module] -> [String]
    names ms = nub do
      D.Module moduleName ds exts <- ms
      ident <- mapMaybe (getDeclName exts) ds
      qual <- [ Qualified Nothing ident, Qualified (Just moduleName) ident ]
      return (show qual)

    getDeclName :: Maybe [D.DeclarationRef] -> D.Declaration -> Maybe Ident
    getDeclName Nothing (D.ValueDeclaration ident _ _ _ _) = Just ident
    getDeclName (Just exts) (D.ValueDeclaration ident _ _ _ _) | any (exports ident) exts = Just ident
    getDeclName exts (D.PositionedDeclaration _ d) = getDeclName exts d
    getDeclName _ _ = Nothing

    exports :: Ident -> D.DeclarationRef -> Boolean
    exports ident (D.ValueRef ident') = ident == ident'
    exports ident (D.PositionedDeclarationRef _ r) = exports ident r
    exports _ _ = false

    isPrefixOf :: String -> String -> Boolean
    isPrefixOf s1 s2 = indexOf s1 s2 == 0

    lastIdent :: String -> Maybe String
    lastIdent = last <<< filter (\s -> length s > 0) <<< match (regex "[A-Za-z0-9.]*" (parseFlags "g"))

handleCommand :: [String] -> RefVal PSCIState -> Command -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref, eval :: Eval) Unit
handleCommand _ _ Help = trace help
handleCommand _ _ Quit = trace "See ya!" *> exit 0
handleCommand _ state (TypeOf v) = do
  st <- readRef state
  handleTypeOf st v
handleCommand _ state (Eval v) = do
  st <- readRef state
  handleEval st v
handleCommand _ state (LoadFile filename) = do
  e <- loadModule filename
  case e of
    Left err -> trace err
    Right m -> modifyRef state (\st -> st { loadedModules = st.loadedModules ++ [Tuple filename m] })
handleCommand _ state (Import mn) =
  modifyRef state (\st -> st { importedModuleNames = st.importedModuleNames ++ [mn] })
handleCommand initialFiles state Reset = do
  e <- sequence <$> traverse loadModule initialFiles
  case e of
    Left err -> trace err
    Right ms -> writeRef state (emptyPSCIState (zip initialFiles ms))
handleCommand _ state (Let f) =
  modifyRef state (\st -> st { letBindings = st.letBindings ++ [f] })

lineHandler :: [String] -> RefVal PSCIState -> String -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref, eval :: Eval) Unit
lineHandler initialFiles state input =
  case parseCommand input of
    Left msg -> trace msg
    Right cmd -> handleCommand initialFiles state cmd

loop :: [String] -> Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref, eval :: Eval) Unit
loop inputFiles = do
  let allFiles = preludeFiles ++ [replModule] ++ inputFiles
  e <- sequence <$> traverse loadModule allFiles
  case e of
    Left err -> do
      trace err
      exit 1
    Right allModules -> do
      state <- newRef (emptyPSCIState (zip allFiles allModules))
      interface <- createInterface process.stdin process.stdout (completion state)
      setPrompt "> " 2 interface
      prompt interface
      setLineHandler (\s -> lineHandler allFiles state s <* prompt interface) interface
      return unit

inputFiles :: Args [String]
inputFiles = many argOnly

term :: Args (Eff (fs :: FS, trace :: Trace, process :: Process, console :: Console, ref :: Ref, eval :: Eval) Unit)
term = loop <$> inputFiles

main = do
  trace prologueMessage
  result <- readArgs' term
  case result of
    Left err -> print err
    _ -> return unit
