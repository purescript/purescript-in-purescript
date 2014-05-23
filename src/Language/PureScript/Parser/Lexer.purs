module Language.PureScript.Parser.Lexer where
  
import Data.Either
import Data.String

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
  | Integer Number
  | Float Number
  | Hex Number
  
  | LineComment String
  | BlockComment String
  
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
  show (Integer n)           = "Integer (" ++ show n ++ ")"
  show (Float n)             = "Float (" ++ show n ++ ")"
  show (Hex n)               = "Hex (" ++ show n ++ ")"
  show (LineComment s)       = "LineComment (" ++ show s ++ ")"
  show (BlockComment s)      = "BlockComment (" ++ show s ++ ")"
  
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
  (==) (Integer n1)        (Integer n2)        = n1 == n2
  (==) (Float n1)          (Float n2)          = n1 == n2
  (==) (Hex n1)            (Hex n2)            = n1 == n2
  (==) (LineComment s1)    (LineComment s2)    = s1 == s2
  (==) (BlockComment s1)   (BlockComment s2)   = s1 == s2
  (==) _                   _                   = false
  (/=) tok1                tok2                = not (tok1 == tok2)
    
type Indentation = Number 

type Line = Number
type Column = Number 
    
type Position = Number    
    
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
      
lex :: String -> Either String [Token]
lex input = do
  let wh = eatWhitespace 0 []
  ts <- go 1 (wh.next + 1) wh.next []
  removeWhitespace [] ts []
  where
  go :: Line -> Column -> Position -> [Token] -> Either String [Token]
  go _    _   i ts | i >= length input = Right (Data.Array.reverse ts)
  go line col i ts | charAt i input == " " = go line (col + 1) (i + 1) ts
  go line col i ts | charAt i input == "\r" = go line (col + 1) (i + 1) ts
  go line col i ts | charAt i input == "\n" = 
    let wh = eatWhitespace (i + 1) ts
    in go (line + 1) (wh.len + 1) wh.next wh.toks

  go line col i ts | charAt i input == "-" && charAt (i + 1) input == "-" && lookaheadChar (i + 2) (not <<< isSymbolChar) = 
    let tok = eatWhile (i + 2) (\s -> s /= "\n")
    in go line (col + length tok.str) tok.next (LineComment tok.str : ts)
  go line col i ts | charAt i input == "{" && charAt (i + 1) input == "-" = 
    let tok = eatWhile' (i + 2) (\j -> j < length input && ((charAt j input /= "-") || (charAt (j + 1) input /= "}")))
    in go line (col + length tok.str + 2) (tok.next + 2) (BlockComment tok.str : ts)
      
  go line col i ts | charAt i input == "(" = go line (col + 1) (i + 1) (LParen : ts)
  go line col i ts | charAt i input == ")" = go line (col + 1) (i + 1) (RParen : ts)
  go line col i ts | charAt i input == "{" = go line (col + 1) (i + 1) (LBrace : ts)
  go line col i ts | charAt i input == "}" = go line (col + 1) (i + 1) (RBrace : ts)
  go line col i ts | charAt i input == "[" = go line (col + 1) (i + 1) (LSquare : ts)
  go line col i ts | charAt i input == "]" = go line (col + 1) (i + 1) (RSquare : ts)
  go line col i ts | charAt i input == "." && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) (Dot : ts)
  go line col i ts | charAt i input == "<" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) (LAngle : ts)
  go line col i ts | charAt i input == ">" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) (RAngle : ts)
  go line col i ts | charAt i input == ":" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) (Colon : ts)
  go line col i ts | charAt i input == "=" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) (Equals : ts)
  go line col i ts | charAt i input == "|" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) (Pipe : ts)
  go line col i ts | charAt i input == "@" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) (i + 1) (At : ts)
  go line col i ts | charAt i input == "`" = go line (col + 1) (i + 1) (Tick : ts)
  go line col i ts | charAt i input == "," = go line (col + 1) (i + 1) (Comma : ts)
  go line col i ts | charAt i input == ";" = go line (col + 1) (i + 1) (Semi : ts)
  
  go line col i ts | charAt i input == ":" && charAt (i + 1) input == ":" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) (DoubleColon : ts)
  go line col i ts | charAt i input == "<" && charAt (i + 1) input == "-" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) (LArrow : ts)
  go line col i ts | charAt i input == "-" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) (RArrow : ts)
  go line col i ts | charAt i input == "<" && charAt (i + 1) input == "=" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) (LFatArrow : ts)
  go line col i ts | charAt i input == "=" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) (i + 2) (RFatArrow : ts)
  
  go line col i ts | isNumeric (charAt i input) =
    let ns = eatWhile i isNumeric 
    in go line (col + length ns.str) ns.next (Natural (buildNat ns.str 0 0) : ts)
    where
    buildNat :: String -> Number -> Number -> Number
    buildNat s i acc | i >= length s = acc
    buildNat s i acc = buildNat s (i + 1) (acc * 10 + toDigit (charAt i s))
    
    toDigit :: String -> Number
    toDigit "0" = 0
    toDigit "1" = 1
    toDigit "2" = 2
    toDigit "3" = 3
    toDigit "4" = 4
    toDigit "5" = 5
    toDigit "6" = 6
    toDigit "7" = 7
    toDigit "8" = 8
    toDigit "9" = 9
  
  go line col i ts | isIdentStart (charAt i input) = 
    let tok = eatWhile i isIdentChar
        col' = col + length tok.str
    in case tok.str of
      s | shouldIndent tok.str -> 
        let lme = nextLexeme line col' tok.next ts
        in case {} of
          _ | charAt lme.next input == "{" -> go line col' tok.next (LName tok.str : ts)
          _ ->  go lme.line lme.col lme.next (ShouldIndent lme.col : LName tok.str : ts)
      s -> go line col' tok.next (LName tok.str : ts)
    
  go line col i ts | isUpper (charAt i input) = 
    let tok = eatWhile i isIdentChar
    in go line (col + length tok.str) tok.next (UName tok.str : ts)
  go line col i ts | isSymbolChar (charAt i input) = 
    let tok = eatWhile i isSymbolChar
    in go line (col + length tok.str) tok.next (Symbol tok.str : ts)
    
  go line col i ts | charAt i input == "\"" =
    case readStringLiteral (i + 1) of
      Left err -> Left err
      Right tok -> go line (col + tok.count) tok.next (StringLiteral tok.str : ts)
    
  go line col _ _ = Left $ "Lexer error at line " ++ show line ++ ", column " ++ show col
  
  removeWhitespace :: [Indentation] -> [Token] -> [Token] -> Either String [Token]
  removeWhitespace (m : ms) (Newline n : ts)      acc | m == n = removeWhitespace (m : ms) ts (Semi : acc)
  removeWhitespace (m : ms) (Newline n : ts)      acc | n < m = removeWhitespace ms ts (RBrace : acc)
  removeWhitespace ms       (Newline n : ts)      acc = removeWhitespace ms ts acc
  removeWhitespace (m : ms) (ShouldIndent n : ts) acc | n > m = removeWhitespace (n : m : ms) ts (LBrace : acc)
  removeWhitespace []       (ShouldIndent n : ts) acc | n > 1 = removeWhitespace [n] ts (LBrace : acc)
  removeWhitespace ms       (ShouldIndent n : ts) acc = removeWhitespace ms (Newline n : ts) (RBrace : LBrace : acc)
  removeWhitespace (1 : ms) (RBrace : ts)         acc = removeWhitespace ms ts (RBrace : acc)
  removeWhitespace _        (RBrace : ts)         acc = Left "Unexpected }"
  removeWhitespace ms       (LBrace : ts)         acc = removeWhitespace (1 : ms) ts (LBrace : acc)
  removeWhitespace ms       (t : ts)              acc = removeWhitespace ms ts (t : acc)
  removeWhitespace []       []                    acc = return (Data.Array.reverse acc)
  removeWhitespace (m : ms) []                    acc | m > 1 = removeWhitespace ms [] (RBrace : acc)
  
  nextLexeme :: Line -> Column -> Position -> [Token] -> { line :: Line, col :: Column, next :: Position }
  nextLexeme = eat
    where
    eat line col i ts | charAt i input == " " = eat line (col + 1) (i + 1) ts
    eat line col i ts | charAt i input == "\r" = eat line col (i + 1) ts
    eat line col i ts | charAt i input == "\n" = eat (line + 1) 1 (i + 1) ts
    eat line col i ts = { line: line, col: col, next: i }
  
  eatWhitespace :: Position -> [Token] -> { len :: Number, count :: Number, next :: Position, toks :: [Token] }
  eatWhitespace = eat 0 0
    where
    eat len count i ts | charAt i input == " " = eat (len + 1) (count + 1) (i + 1) ts
    eat len count i ts | charAt i input == "\r" = eat len (count + 1) (i + 1) ts
    eat len count i ts = { len: len, count: count, next: i, toks: Newline (len + 1) : ts }
    
  eatWhile :: Position -> (String -> Boolean) -> { next :: Position, str :: String }
  eatWhile start p = eatWhile' start (\i -> p (charAt i input))
    
  eatWhile' :: Position -> (Number -> Boolean) -> { next :: Position, str :: String }
  eatWhile' start p = eat start 0
    where
    eat i _   | i >= length input = { next: i, str: drop start input }
    eat i len | p i = eat (i + 1) (len + 1)
    eat i len = { next: i, str: take len (drop start input) }
    
  readStringLiteral :: Position -> Either String { next :: Position, str :: String, count :: Number }
  readStringLiteral start = eat false 0 start
    where
    eat :: Boolean -> Number -> Position -> Either String { next :: Position, str :: String, count :: Number }
    eat _     _     i | i >= length input = Left $ "Unterminated string literal"
    eat false count i | charAt i input == "\"" = 
      let escaped = take count (drop start input)
      in unEscape escaped (\s -> Right { next: i + 1, str: s, count: count }) (Left $ "Invalid string literal: " ++ show escaped)
    eat _     count i | charAt i input == "\\" = eat true (count + 1) (i + 1)
    eat _     count i = eat false (count + 1) (i + 1)
  
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
  
  isAlphaNum :: String -> Boolean
  isAlphaNum c = isAlpha c || isNumeric c
  
  isIdentStart :: String -> Boolean
  isIdentStart c = isLower c || (c == "_")
  
  isIdentChar :: String -> Boolean
  isIdentChar c = isAlphaNum c || (c == "_")
  
  isNumericLiteralChar :: String -> Boolean
  isNumericLiteralChar c = isNumeric c || (c == ".") || (c == "b") || (c == "x") || (c == "-")
    
  shouldIndent :: String -> Boolean
  shouldIndent "of" = true
  shouldIndent "do" = true
  shouldIndent "where" = true
  shouldIndent "let" = true
  shouldIndent _ = false
