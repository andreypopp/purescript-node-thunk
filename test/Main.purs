module Test.Main where

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Data.Either
import Debug.Trace
import Node.Thunk

main = do
  runThunk (slowTwice 42) print
  runThunk (explode "let's cause an error...") print

slowTwice n = do
  nn <- twice n
  delay 1000
  return nn

foreign import someApi """
  var someApi = {
    twice: function(n, callback) {
      callback(null, 2*n);
    },

    explode: function(str, callback) {
      callback('Oops, something exploded: ' + str);
    }
  };
  """
  :: { twice   :: ThunkFn1 Number Number
     , explode :: ThunkFn1 String Unit
     }

twice = runThunkFn1 someApi.twice
explode = runThunkFn1 someApi.explode
