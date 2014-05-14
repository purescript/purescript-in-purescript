module Language.PureScript.Parser.Lexer where
  
import Data.Either
import Data.String
import Data.String.Regex (regex, Regex(..))

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
  
instance showToken :: Show Token where
  show LParen           = "LParen"
  show RParen           = "RParen"
  show LBrace           = "LBrace"
  show RBrace           = "RBrace"
  show LAngle           = "LAngle"
  show RAngle           = "RAngle"
  show LSquare          = "LSquare"
  show RSquare          = "RSquare"
  show (LName s)        = "LName (" ++ show s ++ ")"
  show (UName s)        = "UName (" ++ show s ++ ")"
  show (Symbol s)       = "Symbol (" ++ show s ++ ")"
  show Indent           = "Indent"
  show Dedent           = "Dedent"
  show LArrow           = "LArrow"
  show RArrow           = "RArrow"
  show LFatArrow        = "LFatArrow"
  show RFatArrow        = "RFatArrow"
  show Colon            = "Colon"
  show Equals           = "Equals"
  show Pipe             = "Pipe"
  show Tick             = "Tick"
    
lex :: String -> Either String [Token]
lex input = 
  let wh = eatWhitespace 0
  in Data.Array.reverse <$> go wh.len wh.len wh.next []
  where
  go :: Number -> Number -> Number -> [Token] -> Either String [Token]
  go _ _ i ts | i >= length input = Right ts
  go ref ind i ts | charAt i input == " " = go ref ind (i + 1) ts
  go ref ind i ts | charAt i input == "\r" = go ref ind (i + 1) ts
  go ref ind i ts | charAt i input == "\n" = case eatWhitespace (i + 1) of
                                               wh | wh.len < ref -> go ind wh.len wh.next (Dedent : ts)
                                               wh | wh.len > ref -> go ind wh.len wh.next (Indent : ts)
                                               wh -> go ind wh.len wh.next ts
  go ref ind i ts | charAt i input == "(" = go ref ind (i + 1) (LParen : ts)
  go ref ind i ts | charAt i input == ")" = go ref ind (i + 1) (RParen : ts)
  go ref ind i ts | charAt i input == "{" = go ref ind (i + 1) (LBrace : ts)
  go ref ind i ts | charAt i input == "}" = go ref ind (i + 1) (RBrace : ts)
  go ref ind i ts | charAt i input == "[" = go ref ind (i + 1) (LSquare : ts)
  go ref ind i ts | charAt i input == "]" = go ref ind (i + 1) (RSquare : ts)
  go ref ind i ts | charAt i input == "<" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go ref ind (i + 1) (LAngle : ts)
  go ref ind i ts | charAt i input == ">" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go ref ind (i + 1) (RAngle : ts)
  go _ _ i _ = Left $ "Parse error at location " ++ show i ++ ": " ++ show (take 20 (drop i input))
  
  eatWhitespace :: Number -> { len :: Number, count :: Number, next :: Number }
  eatWhitespace = eat 0 0
    where
    eat len count i | charAt i input == " " = eat (len + 1) (count + 1) (i + 1)
    eat len count i | charAt i input == "\r" = eat len (count + 1) (i + 1)
    eat len count i = { len: len, count: count, next: i }
  
  lookaheadChar :: Number -> (String -> Boolean) -> Boolean
  lookaheadChar i pred = pred (charAt i input)
  
  isSymbolChar :: String -> Boolean
  isSymbolChar c = indexOf c opCharsString >= 0
  
  lnameRegex :: Regex
  lnameRegex = regex "^[a-z,0-9,_]*" "g"
  
  unameRegex :: Regex
  unameRegex = regex "^[A-Z][a-z,0-9,_]*" "g"
  
  symbolRegex :: Regex
  symbolRegex = regex ("^[" ++ opCharsString ++ "]*") "g"
  
  