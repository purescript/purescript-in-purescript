module Language.PureScript.Options where

import Data.Maybe

-- |
-- The data type of compiler 
--
data Options = Options {
    -- |
    -- Disable inclusion of the built in Prelude
    --
    noPrelude :: Boolean
    -- |
    -- Disable tail-call elimination
    --
  , noTco :: Boolean
    -- |
    -- Perform type checks at runtime
    --
  , performRuntimeTypeChecks :: Boolean
    -- |
    -- Disable inlining of calls to return and bind for the Eff monad
    --
  , noMagicDo :: Boolean
    -- |
    -- When specified, checks the type of `main` in the module, and generate a call to run main
    -- after the module definitions.
    --
  , main :: Maybe String
    -- |
    -- Skip all optimizations
    --
  , noOptimizations :: Boolean
    -- |
    -- Specify the namespace that PureScript modules will be exported to when running in the
    -- browser.
    --
  , browserNamespace :: Maybe String
    -- |
    -- The modules to keep while enabling dead code elimination
    --
  , modules :: [String]
    -- |
    -- The modules to code gen
    --
  , codeGenModules :: [String]
    -- |
    -- Verbose error message
    --
  , verboseErrors :: Boolean
  }

-- |
-- Default compiler 
--
defaultOptions :: Options
defaultOptions = Options { noPrelude: false
                         , noTco: false
                         , performRuntimeTypeChecks: false
                         , noMagicDo: false
                         , main: Nothing
                         , noOptimizations: false
                         , browserNamespace: Nothing
                         , modules: []
                         , codeGenModules: []
                         , verboseErrors: false
                         }
