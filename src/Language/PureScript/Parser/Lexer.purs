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
lex input = Data.Array.reverse <$> go 0 []
  where
  go :: Number -> [Token] -> Either String [Token]
  go i ts | i >= length input = Right ts
  go i ts | charAt i input == "(" = go (i + 1) (LParen : ts)
  go i ts | charAt i input == ")" = go (i + 1) (RParen : ts)
  go i ts | charAt i input == "{" = go (i + 1) (LBrace : ts)
  go i ts | charAt i input == "}" = go (i + 1) (RBrace : ts)
  go i ts | charAt i input == "[" = go (i + 1) (LSquare : ts)
  go i ts | charAt i input == "]" = go (i + 1) (RSquare : ts)
  go i ts | charAt i input == "<" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go (i + 1) (LAngle : ts)
  go i ts | charAt i input == ">" && lookaheadChar (i + 1) (not <<< isSymbolChar) = go (i + 1) (RAngle : ts)
  go i _ = Left $ "Parse error at location " ++ show i ++ ": '" ++ take 20 (drop i input) ++ "...'"
  
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
  
  