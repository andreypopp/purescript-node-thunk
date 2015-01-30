# Node.Thunk

`Node.Thunk` is a library for [PureScript][purescript] which helps to interface
with asynchronous Node.js code:

## Wrapping Node.js async code with FFI and thunks

This simple example shows how to use `Node.Thunk` to wrap a part of Node.js's
`fs` module:

    import Node.Thunk

    foreign import fs "var fs = require('fs');" :: {
      readFile :: ThunkFn1 String String
    }

    readFile = runThunkFn1 fs.readFile

The type `ThunkFn1 a b` means that a function takes a single argument of type
`a` and produces a result of type `b`. To convert `ThunkFn1 a b` into a
PureScript function `a -> Thunk b` we need to apply `runThunkFn1` function.

Similarly to `ThunkFn1` there exists `ThunkFn2`, `ThunkFn3` for each arity up to
5.

## Computation with thunks

This simple examples shows how to combine computations with thunks:

    readFileAndWait = do
      contents <- readFile "./README.md"
      delay 1000
      return contents

`delay` is a computation which results into `Unit` value (nothing) after some
milliseconds elapsed.

To execute computation you should use `runThunk` function:

    main = runThunk readFileAndWait handle
        where
      handle (Left err) = -- handle error
      handle (Right result) = -- handle result

`runThunk` function thunk as first argument and handler as second argument.
Handler is supplied with either an error or a result.

## Executing `Eff` actions inside thunk computations

There's a function `liftEff` which can lift `Eff` action into `Thunk`:

    readFileAndPrintContents = do
      contents <- readFile "./README.md"
      liftEff (print contents)


[purescript]: http://purescript.org
