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
  
  | LName String
  | UName String
  | Symbol String
  
  | Indent
  | Dedent

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
  
  | StringLiteral String

  | Natural Number
  | Integer Number
  | Float Number
  | Hex Number
  
instance showToken :: Show Token where
  show LParen                = "LParen"
  show RParen                = "RParen"
  show LBrace                = "LBrace"
  show RBrace                = "RBrace"
  show LAngle                = "LAngle"
  show RAngle                = "RAngle"
  show LSquare               = "LSquare"
  show RSquare               = "RSquare"
  show (LName s)             = "LName (" ++ show s ++ ")"
  show (UName s)             = "UName (" ++ show s ++ ")"
  show (Symbol s)            = "Symbol (" ++ show s ++ ")"
  show Indent                = "Indent"
  show Dedent                = "Dedent"
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
  show (StringLiteral s)     = "StringLiteral (" ++ show s ++ ")"
  show (Natural n)           = "Natural (" ++ show n ++ ")"
  show (Integer n)           = "Integer (" ++ show n ++ ")"
  show (Float n)             = "Float (" ++ show n ++ ")"
  show (Hex n)               = "Hex (" ++ show n ++ ")"
    
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
lex input = 
  let wh = eatWhitespace 0 0 []
  in Data.Array.reverse <$> go 1 (wh.next + 1) wh.len wh.next []
  where
  go :: Line -> Column -> Indentation -> Position -> [Token] -> Either String [Token]
  go _    _   _   i ts | i >= length input = Right ts
  go line col ref i ts | charAt i input == " " = go line (col + 1) ref (i + 1) ts
  go line col ref i ts | charAt i input == "\r" = go line (col + 1) ref (i + 1) ts
  go line col ref i ts | charAt i input == "\n" = 
    let wh = eatWhitespace (i + 1) ref ts
    in go (line + 1) (wh.len + 1) wh.len wh.next wh.toks
      
  go line col ref i ts | charAt i input == "(" = go line (col + 1) ref (i + 1) (LParen : ts)
  go line col ref i ts | charAt i input == ")" = go line (col + 1) ref (i + 1) (RParen : ts)
  go line col ref i ts | charAt i input == "{" = go line (col + 1) ref (i + 1) (LBrace : ts)
  go line col ref i ts | charAt i input == "}" = go line (col + 1) ref (i + 1) (RBrace : ts)
  go line col ref i ts | charAt i input == "[" = go line (col + 1) ref (i + 1) (LSquare : ts)
  go line col ref i ts | charAt i input == "]" = go line (col + 1) ref (i + 1) (RSquare : ts)
  go line col ref i ts | charAt i input == "." && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) ref (i + 1) (Dot : ts)
  go line col ref i ts | charAt i input == "<" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) ref (i + 1) (LAngle : ts)
  go line col ref i ts | charAt i input == ">" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) ref (i + 1) (RAngle : ts)
  go line col ref i ts | charAt i input == ":" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) ref (i + 1) (Colon : ts)
  go line col ref i ts | charAt i input == "=" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) ref (i + 1) (Equals : ts)
  go line col ref i ts | charAt i input == "|" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go line (col + 1) ref (i + 1) (Pipe : ts)
  go line col ref i ts | charAt i input == "`" = go line (col + 1) ref (i + 1) (Tick : ts)
  go line col ref i ts | charAt i input == "," = go line (col + 1) ref (i + 1) (Comma : ts)

  go line col ref i ts | charAt i input == ":" && charAt (i + 1) input == ":" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) ref (i + 2) (DoubleColon : ts)
  go line col ref i ts | charAt i input == "<" && charAt (i + 1) input == "-" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) ref (i + 2) (LArrow : ts)
  go line col ref i ts | charAt i input == "-" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) ref (i + 2) (RArrow : ts)
  go line col ref i ts | charAt i input == "<" && charAt (i + 1) input == "=" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) ref (i + 2) (LFatArrow : ts)
  go line col ref i ts | charAt i input == "=" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go line (col + 2) ref (i + 2) (RFatArrow : ts)
  
  go line col ref i ts | isNumeric (charAt i input) =
    let ns = eatWhile i isNumeric 
    in go line (col + length ns.str) ref ns.next (Natural (buildNat ns.str 0 0) : ts)
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
  
  go line col ref i ts | isIdentStart (charAt i input) = 
    let tok = eatWhile i isIdentChar
    in go line (col + length tok.str) ref tok.next (LName tok.str : ts)
  go line col ref i ts | isUpper (charAt i input) = 
    let tok = eatWhile i isIdentChar
    in go line (col + length tok.str) ref tok.next (UName tok.str : ts)
  go line col ref i ts | isSymbolChar (charAt i input) = 
    let tok = eatWhile i isSymbolChar
    in go line (col + length tok.str) ref tok.next (Symbol tok.str : ts)
    
  go line col ref i ts | charAt i input == "\"" =
    -- TODO: make this tail recursive 
    case readStringLiteral (i + 1) of
      Left err -> Left err
      Right tok -> go line (col + tok.count) ref tok.next (StringLiteral tok.str : ts)
    
  go line col _ i _ = Left $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ show (take 20 (drop i input))
  
  eatWhitespace :: Position -> Indentation -> [Token] -> { len :: Number, count :: Number, next :: Position, toks :: [Token] }
  eatWhitespace = eat 0 0
    where
    eat len count i ref ts | charAt i input == " " = eat (len + 1) (count + 1) (i + 1) ref ts
    eat len count i ref ts | charAt i input == "\r" = eat len (count + 1) (i + 1) ref ts
    eat len count i ref ts | len > ref = { len: len, count: count, next: i, toks: Indent : ts }
    eat len count i ref ts | len < ref = { len: len, count: count, next: i, toks: Dedent : ts }
    eat len count i _   ts = { len: len, count: count, next: i, toks: ts }
    
  eatWhile :: Position -> (String -> Boolean) -> { next :: Position, str :: String }
  eatWhile start p = eat start 0
    where
    eat i _   | i >= length input = { next: i, str: drop start input }
    eat i len | p (charAt i input) = eat (i + 1) (len + 1)
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
    
