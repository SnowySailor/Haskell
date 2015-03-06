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

-- We can use the <- operator to get a value from a function and bind it to a varaible. So name <- getLine would bind the result of you entering
-- something to the variable "name". Think of it like a box. The <- allows you to access what's inside the box that is allowed to go outside the 
-- "clean" place that is the code. 

-- Created fortune.hs. 
-- We include a function in the putStrLn because it just returns a string, so we can print it back out no problem. 
-- You can't say something like "putStrLn "hello there" ++ getLine" because getLine returns type IO String and not String, so you can't combine.
-- Every IO action has a result that is associated with it. We could have written foo <- putStrLn "What's your name?" but it wouldn't have done 
-- anything. But we could have done it. 

-- In a 'do' block, the last action can't be bound to anything. 
-- You can use IO actions in GHCI just fine without having to compile a program. 

-- Created toupper.hs
-- We can use let bindings just like we did in list comprehentions in that we don't need a 'where' as well. So we can just define a new expression
-- and then use it later on in the 'do' block. 
-- It's good practice to line things up correctly indentation-wise becaue indentation is important in Haskell. 

-- Created reverse.hs
-- Now we're going to create a program that will reverse the words that we put in and then die when we enter a blank line. 


-- Protip: To run a program you can either compile it and then run the produced executable file by doing 
-- ghc --make helloworld and then ./helloworld 
-- or you can use the runhaskell command like so: runhaskell helloworld.hs and your program will be executed on the fly.