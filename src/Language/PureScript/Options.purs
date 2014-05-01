module Language.PureScript.Options where

import Data.Maybe

-- |
-- The data type of compiler options
--
data Options = Options {
    -- |
    -- Disable inclusion of the built in Prelude
    --
    optionsNoPrelude :: Boolean
    -- |
    -- Disable tail-call elimination
    --
  , optionsNoTco :: Boolean
    -- |
    -- Perform type checks at runtime
    --
  , optionsPerformRuntimeTypeChecks :: Boolean
    -- |
    -- Disable inlining of calls to return and bind for the Eff monad
    --
  , optionsNoMagicDo :: Boolean
    -- |
    -- When specified, checks the type of `main` in the module, and generate a call to run main
    -- after the module definitions.
    --
  , optionsMain :: Maybe String
    -- |
    -- Skip all optimizations
    --
  , optionsNoOptimizations :: Boolean
    -- |
    -- Specify the namespace that PureScript modules will be exported to when running in the
    -- browser.
    --
  , optionsBrowserNamespace :: Maybe String
    -- |
    -- The modules to keep while enabling dead code elimination
    --
  , optionsModules :: [String]
    -- |
    -- The modules to code gen
    --
  , optionsCodeGenModules :: [String]
    -- |
    -- Verbose error message
    --
  , optionsVerboseErrors :: Boolean
  }

-- |
-- Default compiler options
--
defaultOptions :: Options
defaultOptions = Options { optionsNoPrelude: false
                         , optionsNoTco: false
                         , optionsPerformRuntimeTypeChecks: false
                         , optionsNoMagicDo: false
                         , optionsMain: Nothing
                         , optionsNoOptimizations: false
                         , optionsBrowserNamespace: Nothing
                         , optionsModules: []
                         , optionsCodeGenModules: []
                         , optionsVerboseErrors: false
                         }
