# Node.Thunk

`Node.Thunk` is a library for [PureScript][purescript] which helps to interface
with asynchronous Node.js code:

    import Node.Thunk

    --
    -- Define incomplete interface to Node.js's fs module
    --
    foreign import fs "var fs = require('fs');" :: {
      readFile :: ThunkFn2 String String,
      writeFile :: ThunkFn3 String String String
      }

    readFile = runThunkFn2 fs.readFile
    writeFile = runThunkFn3 fs.writeFile

    --
    -- Some computation with thunks, delay is just a setTimeout
    --
    printA = do
      liftEff $ print "start a"
      delay 1000
      liftEff $ print "a"

    printB = do
      liftEff $ print "start b"
      delay 1000
      liftEff $ print "b"

    printC = do
      liftEff $ print "start c"
      delay 1000
      liftEff $ print "c"

    ---
    --- A main computation
    ---
    computation = do

      -- note we use liftEff to execute Eff actions inside Thunk computation
      liftEff $ print "wait..."
      delay 1000
      liftEff $ print "go!"

      -- read a file and print it on console
      contents <- readFile "./src/Node/Thunk.purs" "utf8"
      liftEff $ trace contents

      -- now we start executing printA, printB and printC in parallel
      par printA (par printB printC)

    --
    -- We use runThunk to start executing Thunk computations inside Eff
    -- computation.
    --
    main = runThunk computation handle
        where
      handle (Left err) = print ("Error: " ++ (show err))
      handle (Right result) = print ("Result: " ++ (show result))

[purescript]: http://purescript.org
