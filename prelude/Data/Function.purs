module Data.Function where

on :: forall a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = g x `f` g y

foreign import data Fn0 :: * -> *
foreign import data Fn1 :: * -> * -> *
foreign import data Fn2 :: * -> * -> * -> *
foreign import data Fn3 :: * -> * -> * -> * -> *
foreign import data Fn4 :: * -> * -> * -> * -> * -> *
foreign import data Fn5 :: * -> * -> * -> * -> * -> * -> *

foreign import mkFn0
  "function mkFn0(f) {\
  \  return function() {\
  \    return f({});\
  \  };\
  \}" :: forall a. (Unit -> a) -> Fn0 a

foreign import mkFn1
  "function mkFn1(f) {\
  \  return function(a) {\
  \    return f(a);\
  \  };\
  \}" :: forall a b. (a -> b) -> Fn1 a b

foreign import mkFn2
  "function mkFn2(f) {\
  \  return function(a, b) {\
  \    return f(a)(b);\
  \  };\
  \}" :: forall a b c. (a -> b -> c) -> Fn2 a b c

foreign import mkFn3
  "function mkFn3(f) {\
  \  return function(a, b, c) {\
  \    return f(a)(b)(c);\
  \  };\
  \}" :: forall a b c d. (a -> b -> c -> d) -> Fn3 a b c d

foreign import mkFn4
  "function mkFn4(f) {\
  \  return function(a, b, c, d) {\
  \    return f(a)(b)(c)(d);\
  \  };\
  \}" :: forall a b c d e. (a -> b -> c -> d -> e) -> Fn4 a b c d e

foreign import mkFn5
  "function mkFn5(f) {\
  \  return function(a, b, c, d, e) {\
  \    return f(a)(b)(c)(d)(e);\
  \  };\
  \}" :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Fn5 a b c d e f

foreign import runFn0
  "function runFn0(f) {\
  \  return f();\
  \}" :: forall a. Fn0 a -> a

foreign import runFn1
  "function runFn1(f) {\
  \  return function(a) {\
  \    return f(a);\
  \  };\
  \}" :: forall a b. Fn1 a b -> a -> b

foreign import runFn2
  "function runFn2(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return f(a, b);\
  \    };\
  \  };\
  \}" :: forall a b c. Fn2 a b c -> a -> b -> c

foreign import runFn3
  "function runFn3(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return f(a, b, c);\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d. Fn3 a b c d -> a -> b -> c -> d

foreign import runFn4
  "function runFn4(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return f(a, b, c, d);\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e. Fn4 a b c d e -> a -> b -> c -> d -> e

foreign import runFn5
  "function runFn5(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return f(a, b, c, d, e);\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}" :: forall a b c d e f. Fn5 a b c d e f -> a -> b -> c -> d -> e -> f
