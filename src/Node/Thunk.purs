module Node.Thunk where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Either
import Data.Function

--
-- Thunk is represented as
--   function(cb) {
--     ...
--   }
-- where cb is expected to be function(err, result) { ... }
--
foreign import data Thunk :: * -> *

foreign import resolve
  """
  function resolve(a) {
    return function(cb) { cb(null, a); };
  }
  """
  :: forall a. a -> Thunk a

foreign import reject
  """
  function reject(err) {
    return function(cb) { cb(err); };
  }
  """
  :: forall a. Error -> Thunk a

foreign import _runThunk
  """
  function _runThunk(left, right, thunk, handler) {
    return function() {
      return thunk(function(err, result) {
        if (err) {
          handler(left(err))();
        } else {
          handler(right(result))();
        }
      });
    };
  }
  """
  :: forall a b eff eff2.  Fn4
    (Error -> Either Error a)
    (a -> Either Error a)
    (Thunk a)
    (Either Error a -> Eff (eff) b)
    (Eff (eff2) Unit)

runThunk = runFn4 _runThunk Left Right

foreign import fmap
  """
  function fmap(f) {
    return function(a) {
      return function(cb) {
        a(function(err, result) {
          if (err) return cb(err);
          try {
            result = f(result);
          } catch (err) {
            return cb(err);
          }
          cb(null, result);
        });
      };
    };
  }
  """
  :: forall a b. (a -> b) -> Thunk a -> Thunk b

foreign import app
  """
  function app(f) {
    return function(a) {
      return function(cb) {
        var latch = 2;
        var fVal, aVal;

        f(function(err, f) {
          if (err && latch !== 0) {
            latch = 0;
            return cb(err);
          }
          latch = latch - 1;
          fVal = f;
          if (latch === 0) {
            try {
              aVal = fVal(aVal);
            } catch(err) {
              return cb(err);
            }
              cb(null, aVal);
          }
        });

        a(function(err, a) {
          if (err && latch !== 0) {
            latch = 0;
            return cb(err);
          }
          latch = latch - 1;
          aVal = a;
          if (latch === 0) {
            try {
              aVal = fVal(aVal);
            } catch(err) {
              return cb(err);
            }
            cb(null, aVal);
          }
        });
      };
    };
  }
  """
  :: forall a b. Thunk (a -> b) -> Thunk a -> Thunk b

foreign import bind
  """
  function bind(a) {
    return function(f) {
      return function(cb) {
        a(function(err, a) {
          if(err) return cb(err);
          try {
            f = f(a);
          } catch(err) {
            return cb(err);
          }
          f(cb);
        });
      }
    }
  }
  """
  :: forall a b. Thunk a -> (a -> Thunk b) -> Thunk b

foreign import delay
  """
  function delay(ms) {
    return function(cb) {
      setTimeout(function() {
        cb(null);
      }, ms);
    };
  }
  """
  :: Number -> Thunk Unit

foreign import liftEff
  """
  function liftEff(action) {
     return function(cb) {
       try {
         cb(null, action());
       } catch(err) {
         cb(err);
       }
     }
  }
  """
  :: forall a eff. Eff (eff) a -> Thunk a

liftEither :: forall a. Either Error a -> Thunk a
liftEither (Left err) = reject err
liftEither (Right result) = resolve result

instance thunkFunctor :: Functor Thunk where
  (<$>) = fmap

instance thunkApply :: Apply Thunk where
  (<*>) = app

instance thunkApplication :: Applicative Thunk where
  pure = resolve

instance thunkBind :: Bind Thunk where
  (>>=) = bind

instance thunkMonad :: Monad Thunk

foreign import data ThunkFn1 :: * -> * -> *
foreign import data ThunkFn2 :: * -> * -> * -> *
foreign import data ThunkFn3 :: * -> * -> * -> * -> *
foreign import data ThunkFn4 :: * -> * -> * -> * -> * -> *
foreign import data ThunkFn5 :: * -> * -> * -> * -> * -> * -> *

foreign import runThunkFn1
  """
  function runThunkFn1(f) {
    return function(a) {
      return function(cb) { return f(a, cb); };
    };
  }
  """
  :: forall a r. ThunkFn1 a r -> (a -> Thunk r)

foreign import runThunkFn2
  """
  function runThunkFn2(f) {
    return function(a) {
      return function(b) {
        return function(cb) { return f(a, b, cb); };
      };
    };
  }
  """
  :: forall a b r. ThunkFn2 a b r -> (a -> b -> Thunk r)

foreign import runThunkFn3
  """
  function runThunkFn3(f) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(cb) {
            return f(a, b, c, cb);
          };
        };
      };
    };
  }
  """
  :: forall a b c r. ThunkFn3 a b c r -> (a -> b -> c -> Thunk r)

foreign import runThunkFn4
  """
  function runThunkFn4(f) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function(cb) {
              return f(a, b, c, d, cb);
            };
          };
        };
      };
    };
  }
  """
  :: forall a b c d r. ThunkFn4 a b c d r -> (a -> b -> c -> d -> Thunk r)

foreign import runThunkFn5
  """
  function runThunkFn5(f) {
    return function(a) {
      return function(b) {
        return function(c) {
          return function(d) {
            return function(e) {
              return function(cb) {
                return f(a, b, c, d, e, cb);
              };
            };
          };
        };
      };
    };
  }
  """
  :: forall a b c d e r. ThunkFn5 a b c d e r -> (a -> b -> c -> d -> e -> Thunk r)

pair :: forall a b m. (Applicative m) => m a -> m b -> m {first :: a, second :: b}
pair a b =
  pure collect <*> a <*> b
    where
  collect first second = {first: first, second: second}

foreign import raise
  """ function raise(err) { return function() { throw err; } }"""
  :: forall eff. Error -> Eff (eff) Unit

unsafeRunThunk thunk = runThunk thunk handle
    where
  handle (Left err) = raise err
  handle (Right result) = return unit
