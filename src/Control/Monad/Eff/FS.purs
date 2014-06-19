module Control.Monad.Eff.FS where

import Control.Monad.Eff
import Global
import Node.FS

foreign import readFile
  "function readFile(filename) {\
  \  return function(k) {\
  \    return function(fail) {\
  \      return function() {\
  \        try {\
  \          return k(require('fs').readFileSync(filename, 'utf8'));\
  \        } catch(err) {\
  \          return fail(err);\
  \        }\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. String -> (String -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r

foreign import writeFile
  "function writeFile(filename) {\
  \  return function(data) {\
  \    return function(k) {\
  \      return function(fail) {\
  \        return function() {\
  \          try {\
  \            return k(require('fs').writeFileSync(filename, data, 'utf8'));\
  \          } catch(err) {\
  \            return fail(err);\
  \          }\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. String -> String -> (Unit -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r

foreign import doesFileExist
  "function doesFileExist(filename) {\
  \  return function(k) {\
  \    return function(fail) {\
  \      return function() {\
  \        try {\
  \          return k(require('fs').existsSync(filename));\
  \        } catch(err) {\
  \          return fail(err);\
  \        }\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. String -> (Boolean -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r

foreign import getModificationTime
  "function getModificationTime(filename) {\
  \  return function(k) {\
  \    return function(fail) {\
  \      return function() {\
  \        try {\
  \          var stat = require('fs').statSync(filename);\
  \          return k(stat.mtime.getTime());\
  \        } catch(err) {\
  \          return fail(err);\
  \        }\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. String -> (Number -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r

foreign import mkdirp
  "function mkdirp(filename) {\
  \  return function(k) {\
  \    return function(fail) {\
  \      return function() {\
  \        try {\
  \          return k(require('mkdirp').sync(filename));\
  \        } catch(err) {\
  \          return fail(err);\
  \        }\
  \      };\
  \    };\
  \  };\
  \}" :: forall eff r. String -> (Unit -> r) -> (Error -> r) -> Eff (fs :: FS | eff) r

foreign import dirname
  "function dirname(filename) {\
  \  return require('path').dirname(filename);\
  \}" :: forall eff. String -> String
