-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pos
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Source position information
--
-----------------------------------------------------------------------------

module Language.PureScript.Pos where

-- |
-- Source position information
--
data SourcePos = SourcePos { name :: String, line :: Number, column :: Number }

mkSourcePos :: String -> Number -> Number -> SourcePos
mkSourcePos name line column = SourcePos { name: name, line: line, column: column }

instance showSourcePos :: Show SourcePos where
  show (SourcePos sp) = sp.name ++ 
                        " line " ++ show sp.line ++ 
                        ", column " ++ show sp.column