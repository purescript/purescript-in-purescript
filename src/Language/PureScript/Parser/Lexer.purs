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
  
  | StringLiteral String
  | NumericLiteral Number
  
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
  show Equals                = "Equals"
  show Pipe                  = "Pipe"
  show Tick                  = "Tick"
  show (StringLiteral s)     = "StringLiteral (" ++ show s ++ ")"
    
type Indentation = Number    
    
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
  in Data.Array.reverse <$> go wh.len wh.next []
  where
  go :: Indentation -> Position -> [Token] -> Either String [Token]
  go _ i ts | i >= length input = Right ts
  go ref i ts | charAt i input == " " = go ref (i + 1) ts
  go ref i ts | charAt i input == "\r" = go ref (i + 1) ts
  go ref i ts | charAt i input == "\n" = 
    let wh = eatWhitespace (i + 1) ref ts
    in go wh.len wh.next wh.toks
      
  go ref i ts | charAt i input == "(" = go ref (i + 1) (LParen : ts)
  go ref i ts | charAt i input == ")" = go ref (i + 1) (RParen : ts)
  go ref i ts | charAt i input == "{" = go ref (i + 1) (LBrace : ts)
  go ref i ts | charAt i input == "}" = go ref (i + 1) (RBrace : ts)
  go ref i ts | charAt i input == "[" = go ref (i + 1) (LSquare : ts)
  go ref i ts | charAt i input == "]" = go ref (i + 1) (RSquare : ts)
  go ref i ts | charAt i input == "<" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go ref (i + 1) (LAngle : ts)
  go ref i ts | charAt i input == ">" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go ref (i + 1) (RAngle : ts)
  go ref i ts | charAt i input == ":" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go ref (i + 1) (Colon : ts)
  go ref i ts | charAt i input == "=" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go ref (i + 1) (Equals : ts)
  go ref i ts | charAt i input == "|" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go ref (i + 1) (Pipe : ts)
  go ref i ts | charAt i input == "`" = go ref (i + 1) (Tick : ts)
  
  go ref i ts | charAt i input == "<" && charAt (i + 1) input == "-" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go ref (i + 2) (LArrow : ts)
  go ref i ts | charAt i input == "-" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go ref (i + 2) (RArrow : ts)
  go ref i ts | charAt i input == "<" && charAt (i + 1) input == "=" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go ref (i + 2) (LFatArrow : ts)
  go ref i ts | charAt i input == "=" && charAt (i + 1) input == ">" && lookaheadChar (i + 2) (not <<< isSymbolChar) = go ref (i + 2) (RFatArrow : ts)
  
  go ref i ts | isIdentStart (charAt i input) = 
    let tok = eatWhile i isIdentChar
    in go ref tok.next (LName tok.str : ts)
  go ref i ts | isUpper (charAt i input) = 
    let tok = eatWhile i isIdentChar
    in go ref tok.next (UName tok.str : ts)
  go ref i ts | isSymbolChar (charAt i input) = 
    let tok = eatWhile i isSymbolChar
    in go ref tok.next (Symbol tok.str : ts)
    
  go ref i ts | charAt i input == "\"" =
    -- TODO: make this tail recursive 
    case readStringLiteral (i + 1) of
      Left err -> Left err
      Right tok -> go ref tok.next (StringLiteral tok.str : ts)
    
  go _ i _ = Left $ "Parse error at location " ++ show i ++ ": " ++ show (take 20 (drop i input))
  
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
    
  readStringLiteral :: Position -> Either String { next :: Position, str :: String }
  readStringLiteral start = eat false 0 start
    where
    eat :: Boolean -> Number -> Position -> Either String { next :: Position, str :: String }
    eat _      _    i | i >= length input = Left $ "Unterminated string literal" ++ show i
    eat false count i | charAt i input == "\"" = 
      let escaped = take count (drop start input)
      in unEscape escaped (\s -> Right { next: i + 1, str: s }) (Left $ "Invalid string literal: " ++ show escaped)
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
    