module Language.PureScript.Options where

import Prelude
import Data.Maybe

-- |
-- The data type of compiler options
--
data Options = Options {
    -- |
    -- Perform tail-call elimination
    --
    optionsTco :: Boolean
    -- |
    -- Perform type checks at runtime
    --
  , optionsPerformRuntimeTypeChecks :: Boolean
    -- |
    -- Inline calls to ret and bind for the Eff monad
    --
  , optionsMagicDo :: Boolean
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
  , optionsBrowserNamespace :: String
    -- |
    -- The modules to keep while enabling dead code elimination
    --
  , optionsModules :: [String]
    -- |
    -- The modules to code gen
    --
  , optionsCodeGenModules :: [String]
  }

-- |
-- Default compiler options
--
defaultOptions :: Options
defaultOptions = Options { optionsTco: false
                         , optionsPerformRuntimeTypeChecks: false
                         , optionsMagicDo: false
                         , optionsMain: Nothing
                         , optionsNoOptimizations: false
                         , optionsBrowserNamespace: "PS"
                         , optionsModules: []
                         , optionsCodeGenModules: []
                         }
