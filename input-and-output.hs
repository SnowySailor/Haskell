-- INPUT AND OUTPUT --


-- HELLO, WORLD! --

-- Created helloworld.hs in Programs. We define a function "main" to do the function putStrLn with the parameter "Hello, World!"
-- We then compile it like "$ ghc --make [filename minus .hs]" so "$ ghc --make helloworld" and when we run the unix executible file,
-- it tells us "Hello, World!" in the Terminal. 

-- The putStrLn function is defined like this:
{-
putStrLn :: String -> IO ()
-}
-- So it takes a String and returns an I/O action, which is something like reading or writing to the Terminal screen. The I/O will
-- also contain some return value in it.