module Node.Thunk where

import Control.Monad.Eff
import Data.Either
import Global
import Debug.Trace

foreign import data Thunk :: * -> *

foreign import resolve
  "function resolve(a) { return function(cb) { cb(null, a); } }"
  :: forall a. a -> Thunk a

foreign import reject
  "function resolve(err) { return function(cb) { cb(err); } }"
  :: forall a. Error -> Thunk Unit

foreign import runThunk
  "function runThunk(a) {\
  \  return function(handler) {\
  \    return function() {\
  \      return a(function(err, result) {\
  \        if (err) {\
  \          handler(PS.Data_Either.Left(err))();\
  \        } else {\
  \          handler(PS.Data_Either.Right(result))();\
  \        }\
  \      });\
  \    };\
  \  };\
  \}"
  :: forall a b eff eff2. Thunk a -> (Either Error a -> Eff (eff) b) -> Eff (eff2) Unit

foreign import fmap
  "function fmap(f) {\
  \  return function(a) {\
  \    return function(cb) {\
  \      a(function(err, result) {\
  \        if (err) return cb(err);\
  \        try {\
  \          cb(null, f(result));\
  \        } catch (err) {\
  \          cb(err);\
  \        }\
  \      });\
  \    };\
  \  };\
  \}"
  :: forall a b. (a -> b) -> Thunk a -> Thunk b

foreign import app
  "function app(f) {\
  \  return function(a) {\
  \    return function(cb) {\
  \      f(function(err, f) {\
  \        if (err) return cb(err);\
  \        a(function(err, a) {\
  \          if (err) return cb(err);\
  \          try {\
  \            cb(null, f(a));\
  \          } catch(err) {\
  \            cb(err);\
  \          }\
  \        });\
  \      });\
  \    };\
  \  };\
  \}"
  :: forall a b. Thunk (a -> b) -> Thunk a -> Thunk b

foreign import bind
  "function bind(a) {\
  \  return function(f) {\
  \    return function(cb) {\
  \      a(function(err, a) {\
  \        if(err) return cb(err);\
  \        try {\
  \          f(a)(cb);\
  \        } catch(err) {\
  \          cb(err);\
  \        }\
  \      });\
  \    }\
  \  }\
  \}"
  :: forall a b. Thunk a -> (a -> Thunk b) -> Thunk b

foreign import delay
  "function delay(ms) {\
  \  return function(cb) { setTimeout(function() { cb(null); }, ms); }\
  \}"
  :: Number -> Thunk Unit

foreign import liftEff
  "function liftEff(action) {\
  \  return function(cb) {\
  \    try {\
  \      cb(null, action());\
  \    } catch(err) {\
  \      cb(err);\
  \    }\
  \  }\
  \}"
  :: forall a eff. Eff (eff) a -> Thunk a

instance thunkFunctor :: Functor Thunk where
  (<$>) = fmap

instance thunkApply :: Apply Thunk where
  (<*>) = app

instance thunkApplication :: Applicative Thunk where
  pure = resolve

instance thunkBind :: Bind Thunk where
  (>>=) = bind

instance thunkMonad :: Monad Thunk

foreign import data ThunkFn1 :: * -> *
foreign import data ThunkFn2 :: * -> * -> *
foreign import data ThunkFn3 :: * -> * -> * -> *
foreign import data ThunkFn4 :: * -> * -> * -> * -> *
foreign import data ThunkFn5 :: * -> * -> * -> * -> * -> *

foreign import runThunkFn1
  "function runThunkFn1(f) {\
  \  return function(a) {\
  \    return function(cb) { return f(a, cb); };\
  \  };\
  \}"
  :: forall a r. ThunkFn1 a -> (a -> Thunk r)

foreign import runThunkFn2
  "function runThunkFn2(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(cb) { return f(a, b, cb); };\
  \    };\
  \  };\
  \}"
  :: forall a b r. ThunkFn2 a b -> (a -> b -> Thunk r)

foreign import runThunkFn3
  "function runThunkFn3(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(cb) { return f(a, b, c, cb); };\
  \      };\
  \    };\
  \  };\
  \}"
  :: forall a b c r. ThunkFn3 a b c -> (a -> b -> c -> Thunk r)

foreign import runThunkFn4
  "function runThunkFn4(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(cb) { return f(a, b, c, d, cb); };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}"
  :: forall a b c d r. ThunkFn4 a b c d -> (a -> b -> c -> d -> Thunk r)

foreign import runThunkFn5
  "function runThunkFn5(f) {\
  \  return function(a) {\
  \    return function(b) {\
  \      return function(c) {\
  \        return function(d) {\
  \          return function(e) {\
  \            return function(cb) { return f(a, b, c, d, e, cb); };\
  \          };\
  \        };\
  \      };\
  \    };\
  \  };\
  \}"
  :: forall a b c d e r. ThunkFn5 a b c d e -> (a -> b -> c -> d -> e -> Thunk r)

foreign import fs "var fs = require('fs');" :: {
  readFile :: ThunkFn2 String String,
  writeFile :: ThunkFn3 String String String
  }

readFile = runThunkFn2 fs.readFile
writeFile = runThunkFn3 fs.writeFile

computation = do
  liftEff $ print "wait..."
  delay 1000
  liftEff $ print "go!"
  contents <- readFile "./src/Node/Thunk.purs" "utf8"
  liftEff $ trace contents
  x <- resolve(1)
  return (x + 1)

main = runThunk computation handle
    where
  handle (Left err) = print ("Error: " ++ (show err))
  handle (Right result) = print ("Result: " ++ (show result))
