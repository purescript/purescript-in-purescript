-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Generates Markdown documentation from PureScript source files.
--
-----------------------------------------------------------------------------

module DocGen where

import Data.Maybe
import Data.Array
import Data.Tuple
import Data.Either
import Data.String (joinWith, split)
import Data.Function (on)
import Data.Foldable
import Data.Traversable

import qualified Language.PureScript.Types as P
import qualified Language.PureScript.Names as P
import qualified Language.PureScript.Errors as P
import qualified Language.PureScript.Environment as P
import qualified Language.PureScript.Declarations as P
import qualified Language.PureScript.Pretty.Types as P
import qualified Language.PureScript.Pretty.Kinds as P

import qualified Language.PureScript.Parser.Lexer as Parser
import qualified Language.PureScript.Parser.Common as Parser
import qualified Language.PureScript.Parser.Declarations as Parser

import Debug.Trace

import Control.Apply
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Process
import Control.Monad.Eff.FS
import Control.Monad.Application

import Control.Monad.Writer
import Control.Monad.Writer.Class

import Control.Monad.Error.Class

import Node.Args
import Node.FS

docgen :: [String] -> Maybe String -> Eff (fs :: FS, trace :: Trace, process :: Process) Unit
docgen input output = runApplication do
  ms <- readInput input
  let docs = runDocs $ renderModules ms
  case output of
    Nothing -> effApplication (trace docs)
    Just filename -> writeFileApplication filename docs
  effApplication (exit 0)

moduleFromText :: String -> Either String P.Module
moduleFromText text = do
  tokens <- Parser.lex text
  Parser.runTokenParser Parser.parseModule tokens

readInput :: forall eff. [String] -> Application [P.Module]
readInput input =
  for input (\inputFile -> do
    text <- readFileApplication inputFile
    case moduleFromText text of
      Left err -> throwError err
      Right m -> return m)

type Docs = Writer [String] Unit

runDocs :: Docs -> String
runDocs = joinWith "\n" <<< execWriter

spacer :: Docs
spacer = tell [""]

replicate :: Number -> String -> String
replicate n s = go n ""
  where
    go 0 acc = acc
    go n acc = go (n - 1) (acc ++ s)

headerLevel :: Number -> String -> Docs
headerLevel level hdr = tell [replicate level "#" ++ " " ++ hdr]

atIndent :: Number -> String -> Docs
atIndent indent text =
  let ls = split "\n" text in
  for_ ls $ \l -> tell [replicate indent " " ++ l]

renderModules :: [P.Module] -> Docs
renderModules ms = do
  headerLevel 1 "Module Documentation"
  spacer
  traverse_ renderModule ms

renderModule :: P.Module -> Docs
renderModule (P.Module moduleName ds exps) =
  let exported = filter (isExported exps) ds
      hasTypes = any isTypeDeclaration ds
      hasTypeclasses = any isTypeClassDeclaration ds
      hasTypeclassInstances = any isTypeInstanceDeclaration ds
      hasValues = any isValueDeclaration ds
  in do
    headerLevel 2 $ "Module " ++ P.runModuleName moduleName
    spacer
    when hasTypes $ do
      headerLevel 3 "Types"
      spacer
      renderTopLevel exps (filter isTypeDeclaration exported)
      spacer
    when hasTypeclasses $ do
      headerLevel 3 "Type Classes"
      spacer
      renderTopLevel exps (filter isTypeClassDeclaration exported)
      spacer
    when hasTypeclassInstances $ do
      headerLevel 3 "Type Class Instances"
      spacer
      renderTopLevel exps (filter isTypeInstanceDeclaration ds)
      spacer
    when hasValues $ do
      headerLevel 3 "Values"
      spacer
      renderTopLevel exps (filter isValueDeclaration exported)
      spacer

isExported :: Maybe [P.DeclarationRef] -> P.Declaration -> Boolean
isExported Nothing _ = true
isExported _ (P.TypeInstanceDeclaration _ _ _ _ _) = true
isExported exps (P.PositionedDeclaration _ d) = isExported exps d
isExported (Just exps) decl = any (matches decl) exps
  where
  matches (P.TypeDeclaration ident _) (P.ValueRef ident') = ident == ident'
  matches (P.ExternDeclaration _ ident _ _) (P.ValueRef ident') = ident == ident'
  matches (P.DataDeclaration ident _ _) (P.TypeRef ident' _) = ident == ident'
  matches (P.ExternDataDeclaration ident _) (P.TypeRef ident' _) = ident == ident'
  matches (P.TypeSynonymDeclaration ident _ _) (P.TypeRef ident' _) = ident == ident'
  matches (P.TypeClassDeclaration ident _ _ _) (P.TypeClassRef ident') = ident == ident'
  matches (P.PositionedDeclaration _ d) r = d `matches` r
  matches d (P.PositionedDeclarationRef _ r) = d `matches` r
  matches _ _ = false

isDctorExported :: P.ProperName -> Maybe [P.DeclarationRef] -> P.ProperName -> Boolean
isDctorExported _ Nothing _ = true
isDctorExported ident (Just exps) ctor = test `any` exps
  where
  test (P.PositionedDeclarationRef _ d) = test d
  test (P.TypeRef ident' Nothing) = ident == ident'
  test (P.TypeRef ident' (Just ctors)) = ident == ident' && ctor `elem` ctors
  test _ = false

renderTopLevel :: Maybe [P.DeclarationRef] -> [P.Declaration] -> Docs
renderTopLevel exps decls = for_ (sortBy (compare `on` getName) decls) $ \decl -> do
  renderDeclaration 4 exps decl
  spacer

renderDeclaration :: Number -> Maybe [P.DeclarationRef] -> P.Declaration -> Docs
renderDeclaration n _ (P.TypeDeclaration ident ty) =
  atIndent n $ show ident ++ " :: " ++ prettyPrintType' ty
renderDeclaration n _ (P.ExternDeclaration _ ident _ ty) =
  atIndent n $ show ident ++ " :: " ++ prettyPrintType' ty
renderDeclaration n exps (P.DataDeclaration name args ctors) = do
  let typeName = P.runProperName name ++ (if null args then "" else " " ++ joinWith " " args)
  let exported = filter (isDctorExported name exps <<< fst) ctors
  atIndent n $ "data " ++ typeName ++ (if null exported then "" else " where")
  for_ exported $ \(Tuple ctor tys) ->
    atIndent (n + 2) $ P.runProperName ctor ++ " :: " ++ joinWith "" (map (\ty -> prettyPrintType' ty ++ " -> ") tys) ++ typeName
renderDeclaration n _ (P.ExternDataDeclaration name kind) =
  atIndent n $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind
renderDeclaration n _ (P.TypeSynonymDeclaration name args ty) = do
  let typeName = P.runProperName name ++ " " ++ joinWith " " args
  atIndent n $ "type " ++ typeName ++ " = " ++ prettyPrintType' ty
renderDeclaration n exps (P.TypeClassDeclaration name args implies ds) = do
  let impliesText = case implies of
                      [] -> ""
                      is -> "(" ++ joinWith ", " (map (\(Tuple pn tys') -> show pn ++ " " ++ joinWith " " (map P.prettyPrintTypeAtom tys')) is) ++ ") <= "
  atIndent n $ "class " ++ impliesText ++ P.runProperName name ++ " " ++ joinWith " " args ++ " where"
  traverse_ (renderDeclaration (n + 2) exps) ds
renderDeclaration n _ (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ joinWith ", " (map (\(Tuple pn tys') -> show pn ++ " " ++ joinWith " " (map P.prettyPrintTypeAtom tys')) cs) ++ ") => "
  atIndent n $ "instance " ++ show name ++ " :: " ++ constraintsText ++ show className ++ " " ++ joinWith " " (map P.prettyPrintTypeAtom tys)
renderDeclaration n exps (P.PositionedDeclaration _ d) =
  renderDeclaration n exps d
renderDeclaration _ _ _ = return unit

prettyPrintType' :: P.Type -> String
prettyPrintType' = P.prettyPrintType <<< P.everywhereOnTypes dePrim
  where
  dePrim ty@(P.TypeConstructor (P.Qualified _ name))
    | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
      P.TypeConstructor $ P.Qualified Nothing name
  dePrim other = other

getName :: P.Declaration -> String
getName (P.TypeDeclaration ident _) = show ident
getName (P.ExternDeclaration _ ident _ _) = show ident
getName (P.DataDeclaration name _ _) = P.runProperName name
getName (P.ExternDataDeclaration name _) = P.runProperName name
getName (P.TypeSynonymDeclaration name _ _) = P.runProperName name
getName (P.TypeClassDeclaration name _ _ _) = P.runProperName name
getName (P.TypeInstanceDeclaration name _ _ _ _) = show name
getName (P.PositionedDeclaration _ d) = getName d
getName _ = P.theImpossibleHappened "Invalid argument to getName"

isValueDeclaration :: P.Declaration -> Boolean
isValueDeclaration (P.TypeDeclaration _ _) = true
isValueDeclaration (P.ExternDeclaration _ _ _ _) = true
isValueDeclaration (P.PositionedDeclaration _ d) = isValueDeclaration d
isValueDeclaration _ = false

isTypeDeclaration :: P.Declaration -> Boolean
isTypeDeclaration (P.DataDeclaration _ _ _) = true
isTypeDeclaration (P.ExternDataDeclaration _ _) = true
isTypeDeclaration (P.TypeSynonymDeclaration _ _ _) = true
isTypeDeclaration (P.PositionedDeclaration _ d) = isTypeDeclaration d
isTypeDeclaration _ = false

isTypeClassDeclaration :: P.Declaration -> Boolean
isTypeClassDeclaration (P.TypeClassDeclaration _ _ _ _) = true
isTypeClassDeclaration (P.PositionedDeclaration _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = false

isTypeInstanceDeclaration :: P.Declaration -> Boolean
isTypeInstanceDeclaration (P.TypeInstanceDeclaration _ _ _ _ _) = true
isTypeInstanceDeclaration (P.PositionedDeclaration _ d) = isTypeInstanceDeclaration d
isTypeInstanceDeclaration _ = false

inputFiles :: Args [String]
inputFiles = many argOnly

outputFile :: Args (Maybe String)
outputFile = opt (flagArg "o" <|> flagArg "output")

term :: Args (Eff (fs :: FS, trace :: Trace, process :: Process) Unit)
term = docgen <$> inputFiles <*> outputFile

main = do
  result <- readArgs' term
  case result of
    Left err -> print err
    _ -> return unit

