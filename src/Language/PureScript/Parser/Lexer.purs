module Language.PureScript.Parser.Lexer where

import Data.Array ()
import Data.Either
import Data.String hiding (charAt)
import qualified Data.String.Unsafe as U
import qualified Data.String.Regex as Rx

import Global (readInt, readFloat)

import Language.PureScript.Keywords (opCharsString)

data Token
  = LParen
  | RParen
  | LBrace
  | RBrace
  | LAngle
  | RAngle
  | LSquare
  | RSquare

  | Newline Indentation
  | ShouldIndent Indentation

  | LArrow
  | RArrow
  | LFatArrow
  | RFatArrow

  | Colon
  | DoubleColon
  | Equals
  | Pipe
  | Tick
  | Dot
  | Comma
  | Semi
  | At

  | LName String
  | UName String
  | Symbol String

  | StringLiteral String

  | Natural Number
  | ANumber Number

instance showToken :: Show Token where
  show LParen                = "LParen"
  show RParen                = "RParen"
  show LBrace                = "LBrace"
  show RBrace                = "RBrace"
  show LAngle                = "LAngle"
  show RAngle                = "RAngle"
  show LSquare               = "LSquare"
  show RSquare               = "RSquare"
  show LArrow                = "LArrow"
  show RArrow                = "RArrow"
  show LFatArrow             = "LFatArrow"
  show RFatArrow             = "RFatArrow"
  show Colon                 = "Colon"
  show DoubleColon           = "DoubleColon"
  show Equals                = "Equals"
  show Pipe                  = "Pipe"
  show Tick                  = "Tick"
  show Dot                   = "Dot"
  show Comma                 = "Comma"
  show Semi                  = "Semi"
  show At                    = "At"
  show (Newline n)           = "Newline (" ++ show n ++ ")"
  show (ShouldIndent n)      = "ShouldIndent (" ++ show n ++ ")"
  show (LName s)             = "LName (" ++ show s ++ ")"
  show (UName s)             = "UName (" ++ show s ++ ")"
  show (Symbol s)            = "Symbol (" ++ show s ++ ")"
  show (StringLiteral s)     = "StringLiteral (" ++ show s ++ ")"
  show (Natural n)           = "Natural (" ++ show n ++ ")"
  show (ANumber n)           = "ANumber (" ++ show n ++ ")"

instance eqToken :: Eq Token where
  (==) LParen              LParen              = true
  (==) RParen              RParen              = true
  (==) LBrace              LBrace              = true
  (==) RBrace              RBrace              = true
  (==) LAngle              LAngle              = true
  (==) RAngle              RAngle              = true
  (==) LSquare             LSquare             = true
  (==) RSquare             RSquare             = true
  (==) LArrow              LArrow              = true
  (==) RArrow              RArrow              = true
  (==) LFatArrow           LFatArrow           = true
  (==) RFatArrow           RFatArrow           = true
  (==) Colon               Colon               = true
  (==) DoubleColon         DoubleColon         = true
  (==) Equals              Equals              = true
  (==) Pipe                Pipe                = true
  (==) Tick                Tick                = true
  (==) Dot                 Dot                 = true
  (==) Comma               Comma               = true
  (==) Semi                Semi                = true
  (==) At                  At                  = true
  (==) (Newline n1)        (Newline n2)        = n1 == n2
  (==) (ShouldIndent n1)   (ShouldIndent n2)   = n1 == n2
  (==) (LName s1)          (LName s2)          = s1 == s2
  (==) (UName s1)          (UName s2)          = s1 == s2
  (==) (Symbol s1)         (Symbol s2)         = s1 == s2
  (==) (StringLiteral s1)  (StringLiteral s2)  = s1 == s2
  (==) (Natural n1)        (Natural n2)        = n1 == n2
  (==) (ANumber n1)        (ANumber n2)        = n1 == n2
  (==) _                   _                   = false
  (/=) tok1                tok2                = not (tok1 == tok2)

type Indentation = Number

type Line = Number
type Column = Number

type Position = Number

type Comments = [String]

type PositionedToken =
  { line :: Line
  , column :: Column
  , token :: Token
  , comments :: Comments
  }

mkPositionedToken :: Line -> Column -> [String] -> Token -> PositionedToken
mkPositionedToken line column comments token = { line: line, column: column, token: token, comments: comments }

foreign import unEscape
  "function unEscape(s) {\
  \  return function (sc) {\
  \    return function (fc) {\
  \      try {\
  \        return sc(JSON.parse('\"' + s + '\"'));\
  \      } catch (ex) {\
  \        return fc;\
  \      }\
  \    };\
  \  };\
  \}" :: forall r. String -> (String -> r) -> r -> r

charAt :: Number -> String -> String
charAt i s = fromChar $ U.charAt i s

lex :: String -> Either String [PositionedToken]
lex input = do
  let wh = eatWhitespace 0 []
  ts <- go 1 (wh.next + 1) wh.next [] []
  removeWhitespace [] ts []
  where
  go :: Line -> Column -> Position -> Comments -> [PositionedToken] -> Either String [PositionedToken]
  go _    _   i _  ts | i >= length input = Right (Data.Array.reverse ts)
  go line col i cs ts | charAt i input == " " = go line (col + 1) (i + 1) cs ts
  go line col i cs ts | charAt i input == "\r" = go line (col + 1) (i + 1) cs ts
  go line col i cs ts | charAt i input == "\n" =
    let wh = eatWhitespace (i + 1) ts
    in go (line + 1) (wh.len + 1) wh.next cs wh.toks

  go line col i cs ts | charAt i input == "-" && charAt (i + 1) input == "-" && lookaheadChar (i + 2) (\x -> (not $ isSymbolChar x) || x == "-") =
    let tok = eatWhile (i + 2) (\s -> s /= "\n")
    in go line (col + length tok.str) tok.next (cs ++ [tok.str]) ts
  go line col i cs ts | charAt i input == "{" && charAt (i + 1) input == "-" =
    let tok = eatWhile' (i + 2) (\j -> j < length input && ((charAt j input /= "-") || (charAt (j + 1) input /= "}")))
    in go line (col + length tok.str + 2) (tok.next + 2) (cs ++ [tok.str]) ts

  go line col i cs ts | charAt i input == "(" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs LParen : ts)
  go line col i cs ts | charAt i input == ")" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs RParen : ts)
  go line col i cs ts | charAt i input == "{" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs LBrace : ts)
  go line col i cs ts | charAt i input == "}" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs RBrace : ts)
  go line col i cs ts | charAt i input == "[" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs LSquare : ts)
  go line col i cs ts | charAt i input == "]" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs RSquare : ts)
  go line col i cs ts | charAt i input == "." && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs Dot : ts)
  go line col i cs ts | charAt i input == "<" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs LAngle : ts)
  go line col i cs ts | charAt i input == ">" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs RAngle : ts)
  go line col i cs ts | charAt i input == ":" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs Colon : ts)
  go line col i cs ts | charAt i input == "=" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs Equals : ts)
  go line col i cs ts | charAt i input == "|" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs Pipe : ts)
  go line col i cs ts | charAt i input == "@" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs At : ts)
  go line col i cs ts | charAt i input == "`" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs Tick : ts)
  go line col i cs ts | charAt i input == "," = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs Comma : ts)
  go line col i cs ts | charAt i input == ";" = go line (col + 1) (i + 1) [] (mkPositionedToken line col cs Semi : ts)

  go line col i cs ts | charAt i input == ":" && charAt (i + 1) input == ":" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) [] (mkPositionedToken line col cs DoubleColon : ts)
  go line col i cs ts | charAt i input == "<" && charAt (i + 1) input == "-" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) [] (mkPositionedToken line col cs LArrow : ts)
  go line col i cs ts | charAt i input == "-" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) [] (mkPositionedToken line col cs RArrow : ts)
  go line col i cs ts | charAt i input == "<" && charAt (i + 1) input == "=" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) [] (mkPositionedToken line col cs LFatArrow : ts)
  go line col i cs ts | charAt i input == "=" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) [] (mkPositionedToken line col cs RFatArrow : ts)

  go line col i cs ts | charAt i input == "0" && charAt (i + 1) input == "x" =
    let ns = eatWhile (i + 2) isHex
    in go line (col + length ns.str) ns.next [] (mkPositionedToken line col cs (ANumber (readInt 16 ns.str)) : ts)

  go line col i cs ts | isNumeric (charAt i input) =
    case readNumberLiteral line col i of
      Left err -> Left err
      Right tok -> go line (col + tok.count) tok.next [] (mkPositionedToken line col cs tok.tok : ts)

  go line col i cs ts | isIdentStart (charAt i input) =
    let tok = eatWhile i isIdentChar
        col' = col + length tok.str
    in case tok.str of
      s | shouldIndent tok.str ->
        let lme = nextLexeme line col' tok.next
        in case unit of
          _ | charAt lme.next input == "{" -> go line col' tok.next [] (mkPositionedToken line col cs (LName tok.str) : ts)
          _ -> go lme.line lme.col lme.next [] (mkPositionedToken line col cs (ShouldIndent lme.col) : mkPositionedToken line col [] (LName tok.str) : ts)
      s -> go line col' tok.next [] (mkPositionedToken line col cs (LName tok.str) : ts)

  go line col i cs ts | isUpper (charAt i input) =
    let tok = eatWhile i isIdentChar
    in go line (col + length tok.str) tok.next [] (mkPositionedToken line col cs (UName tok.str) : ts)
  go line col i cs ts | isSymbolChar (charAt i input) =
    let tok = eatWhile i isSymbolChar
    in go line (col + length tok.str) tok.next [] (mkPositionedToken line col cs (Symbol tok.str) : ts)

  go line col i cs ts | indexOf' "\"\"\"" i input == i =
    case readStringLiteral line col (i + 3) "\"\"\"" replaceNewLines of
      Left err -> Left err
      Right tok -> go line (col + tok.count) tok.next [] (mkPositionedToken line col cs (StringLiteral tok.str) : ts)
    where
    replaceNewLines :: String -> String
    replaceNewLines = joinWith "\\n" <<< split "\n"
  go line col i cs ts | charAt i input == "\"" =
    case readStringLiteral line col (i + 1) "\"" handleMultiline of
      Left err -> Left err
      Right tok -> go line (col + tok.count) tok.next [] (mkPositionedToken line col cs (StringLiteral tok.str) : ts)
    where
    handleMultiline :: String -> String
    handleMultiline = Rx.replace (Rx.regex "\\\\(\\s*)\\n(\\s*)\\\\" (Rx.parseFlags "g")) ""

  go line col _ _ _ = Left $ "Lexer error at line " ++ show line ++ ", column " ++ show col

  removeWhitespace :: [Indentation] -> [PositionedToken] -> [PositionedToken] -> Either String [PositionedToken]
  removeWhitespace ms       ({ token = Newline _ } : (t@{ token = Newline _ }) : ts) acc = removeWhitespace ms (t : ts) acc
  removeWhitespace (m : ms) ((t@{ token = Newline n })      : ts) acc | m == n = removeWhitespace (m : ms) ts (mkPositionedToken t.line t.column t.comments Semi : acc)
  removeWhitespace []       ((t@{ token = Newline 1 })      : ts) acc = removeWhitespace [] ts (mkPositionedToken t.line t.column t.comments Semi : acc)
  removeWhitespace (m : ms) ((t@{ token = Newline n })      : ts) acc | n < m = removeWhitespace ms (t : ts) (mkPositionedToken t.line t.column t.comments RBrace : acc)
  removeWhitespace ms       ((t@{ token = Newline n })      : ts) acc = removeWhitespace ms ts acc
  removeWhitespace (m : ms) ((t@{ token = ShouldIndent n }) : ts) acc | n > m = removeWhitespace (n : m : ms) ts (mkPositionedToken t.line t.column t.comments LBrace : acc)
  removeWhitespace []       ((t@{ token = ShouldIndent n }) : ts) acc | n > 1 = removeWhitespace [n] ts (mkPositionedToken t.line t.column t.comments LBrace : acc)
  removeWhitespace ms       ((t@{ token = ShouldIndent n }) : ts) acc = removeWhitespace ms (mkPositionedToken t.line t.column t.comments (Newline n) : ts) (mkPositionedToken t.line t.column [] RBrace : mkPositionedToken t.line t.column [] LBrace : acc)
  removeWhitespace (1 : ms) ((t@{ token = RBrace })         : ts) acc = removeWhitespace ms ts (mkPositionedToken t.line t.column t.comments RBrace : acc)
  removeWhitespace _        ((t@{ token = RBrace })         : ts) acc = Left "Unexpected }"
  removeWhitespace ms       ((t@{ token = LBrace })         : ts) acc = removeWhitespace (1 : ms) ts (mkPositionedToken t.line t.column t.comments LBrace : acc)
  removeWhitespace ms       (t : ts)                              acc = removeWhitespace ms ts (t : acc)
  removeWhitespace []       []                                    acc = return (Data.Array.reverse acc)
  removeWhitespace (m : ms) []                                    acc | m > 1 = removeWhitespace ms [] (mkPositionedToken 0 0 [] RBrace : acc)

  nextLexeme :: Line -> Column -> Position -> { line :: Line, col :: Column, next :: Position }
  nextLexeme = eat
    where
    eat line col i | charAt i input == " "  = eat line (col + 1) (i + 1)
    eat line col i | charAt i input == "\r" = eat line col (i + 1)
    eat line col i | charAt i input == "\n" = eat (line + 1) 1 (i + 1)
    eat line col i = { line: line, col: col, next: i }

  eatWhitespace :: Position -> [PositionedToken] -> { len :: Number, count :: Number, next :: Position, toks :: [PositionedToken] }
  eatWhitespace = eat 0 0
    where
    eat len count i ts | charAt i input == " " = eat (len + 1) (count + 1) (i + 1) ts
    eat len count i ts | charAt i input == "\r" = eat len (count + 1) (i + 1) ts
    -- TODO: include position info here
    eat len count i ts = { len: len, count: count, next: i, toks: mkPositionedToken 0 0 [] (Newline (len + 1)) : ts }

  eatWhile :: Position -> (String -> Boolean) -> { next :: Position, str :: String }
  eatWhile start p = eatWhile' start (\i -> p (charAt i input))

  eatWhile' :: Position -> (Number -> Boolean) -> { next :: Position, str :: String }
  eatWhile' start p = eat start 0
    where
    eat i _   | i >= length input = { next: i, str: drop start input }
    eat i len | p i = eat (i + 1) (len + 1)
    eat i len = { next: i, str: take len (drop start input) }

  readStringLiteral :: Line -> Column -> Position -> String -> (String -> String) -> Either String { next :: Position, str :: String, count :: Number }
  readStringLiteral line col start term replace = eat false 0 start
    where
    eat :: Boolean -> Number -> Position -> Either String { next :: Position, str :: String, count :: Number }
    eat _     _     i | i >= length input = Left $ "Unterminated string literal at line " ++ show line ++ ", column " ++ show col
    eat false count i | indexOf' term i input == i =
      let escaped = replace (take count (drop start input))
      in unEscape escaped (\s -> Right { next: i + length term, str: s, count: count }) (Left $ "Invalid string literal at line " ++ show line ++ ", column " ++ show col ++ ": " ++ show escaped)
    eat _     count i | charAt i input == "\\" = eat true (count + 1) (i + 1)
    eat _     count i = eat false (count + 1) (i + 1)

  readNumberLiteral :: Line -> Column -> Position -> Either String { next :: Position, tok :: Token, count :: Number }
  readNumberLiteral line col start = eat true false 0 start
    where
    eat :: Boolean -> Boolean -> Number -> Position -> Either String { next :: Position, tok :: Token, count :: Number }
    eat _     true  count i | charAt i input == "." = Left $ "Invalid decimal place in exponent in number literal at line " ++ show line ++ ", column " ++ show col
    eat false _     count i | charAt i input == "." = Left $ "Invalid decimal place in number literal at line " ++ show line ++ ", column " ++ show col
    eat _     _     count i | charAt i input == "." = eat false false (count + 1) (i + 1)
    eat _     true  count i | charAt i input == "e" = Left $ "Invalid exponent in number literal at line " ++ show line ++ ", column " ++ show col
    eat _     _     count i | charAt i input == "e" && charAt (i + 1) input == "-" = eat false true (count + 2) (i + 2)
    eat _     _     count i | charAt i input == "e" && charAt (i + 1) input == "+" = eat false true (count + 2) (i + 2)
    eat _     _     count i | charAt i input == "e" = eat false true (count + 1) (i + 1)
    eat isNat isExp count i | isNumeric (charAt i input) = eat isNat isExp (count + 1) (i + 1)
    eat isNat isExp count i =
      let s = (take count (drop start input))
      in Right { next: i, tok: if isNat then Natural (readInt 10 s) else ANumber (readFloat s), count: count }

  lookaheadChar :: Position -> (String -> Boolean) -> Boolean
  lookaheadChar i pred = pred (charAt i input)

  isSymbolChar :: String -> Boolean
  isSymbolChar c = indexOf c opCharsString >= 0

  isLower :: String -> Boolean
  isLower c = c >= "a" && c <= "z"

  isUpper :: String -> Boolean
  isUpper c = c >= "A" && c <= "Z"

  isAlpha :: String -> Boolean
  isAlpha c = isLower c || isUpper c

  isNumeric :: String -> Boolean
  isNumeric c = c >= "0" && c <= "9"

  isHex :: String -> Boolean
  isHex c = (c >= "a" && c <= "f") || (c >= "A" && c <= "F") || isNumeric c

  isAlphaNum :: String -> Boolean
  isAlphaNum c = isAlpha c || isNumeric c

  isIdentStart :: String -> Boolean
  isIdentStart c = isLower c || (c == "_")

  isIdentChar :: String -> Boolean
  isIdentChar c = isAlphaNum c || (c == "_") || (c == "'")

  isNumericLiteralChar :: String -> Boolean
  isNumericLiteralChar c = isNumeric c || (c == ".") || (c == "b") || (c == "x") || (c == "-")

  shouldIndent :: String -> Boolean
  shouldIndent "of" = true
  shouldIndent "do" = true
  shouldIndent "where" = true
  shouldIndent "let" = true
  shouldIndent _ = false
