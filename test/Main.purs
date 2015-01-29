module Test.Main where

import Control.Monad.Eff
import Data.Either
import Debug.Trace
import Node.Thunk

main = runThunk (slowTwice 42) print

slowTwice n = do
  nn <- twice n
  delay 1000
  return nn

foreign import someApi """
  var someApi = {
    twice: function(n, callback) {
      callback(null, 2*n);
    }
  };
  """ :: { twice :: ThunkFn1 Number Number }

twice = runThunkFn1 someApi.twice

instance errorShow :: Show Error where
  show = showError

foreign import showError """
  function showError(x) {
    return function() {
      return x.toString;
    };
  }
  """ :: Error -> String
