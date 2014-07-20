# Node.Thunk

`Node.Thunk` is a library for [PureScript][purescript] which helps to interface
with asynchronous Node.js code:

    foreign import fs "var fs = require('fs');" :: {
      readFile :: ThunkFn2 String String,
      writeFile :: ThunkFn3 String String String
      }

    readFile = runThunkFn2 fs.readFile
    writeFile = runThunkFn3 fs.writeFile

    printA = do
      liftEff $ print ".a"
      delay 1000
      readFile "./src/Node/Thunk.purs" "utf8"
      liftEff $ print "a"

    printB = do
      liftEff $ print ".b"
      readFile "./src/Node/Thunk.purs" "utf8"
      delay 1000
      liftEff $ print "b"

    printC = do
      liftEff $ print ".c"
      readFile "./src/Node/Thunk.purs" "utf8"
      delay 1000
      liftEff $ print "c"

    computation = do
      liftEff $ print "wait..."
      delay 1000
      liftEff $ print "go!"
      contents <- readFile "./src/Node/Thunk.purs" "utf8"
      liftEff $ trace contents
      x <- resolve(1)
      par printA (par printB printC)
      return (x + 1)

    main = runThunk computation handle
        where
      handle (Left err) = print ("Error: " ++ (show err))
      handle (Right result) = print ("Result: " ++ (show result))

[purescript]: http://purescript.org
