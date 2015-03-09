import Control.Monad
-- INPUT AND OUTPUT --

-- Protip: To run a program you can either compile it and then run the produced executable file by doing 
-- ghc --make helloworld and then ./helloworld 
-- or you can use the runhaskell command like so: runhaskell helloworld.hs and your program will be executed on the fly.


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
-- We can use let bindings just like we did in list comprehentions in that we don't need a 'where' as well. So we can just define a new 
-- expression and then use it later on in the 'do' block. 
-- It's good practice to line things up correctly indentation-wise becaue indentation is important in Haskell. 

-- Created reverse.hs
-- Now we're going to create a program that will reverse the words that we put in and then die when we enter a blank line. 

-- In the main, we have an if/else. Every if must have an else because every expression must have a result. 
-- If our case (that no line was typed) is true, it returns an IO action, and it also returns another IO action if the line is filled with
-- something. That's why your if/else expressions need to have an IO action on both sides. if CONDITION then IO ACTION else IO ACTION.

-- In our "else" we need to have exactly 1 IO Action, so we use a 'do' block to encapsulate that. 

-- We also have a "return" when there is no line to do anything with. This is nothing like languages like Java, C, or PHP. In Haskell, return 
-- makes an IO action out of a pure value. So calling 'return "hahahahaha"' would give us type IO String. It doesn't do anything or cause a do
-- block to end. The program is more than happy to continue on running after even 1000 returns. 
-- Return is like the oposite of <-. return puts a value into a box, and <- takes it out of the box. 
main = do
	a <- return "heck"
	b <- return "yeah"
	putStr' $ a ++ " " ++ b
-- For example, this would output "heck yeah"

-- When dealing with IO do blocks, we mostly use return either to create an IO action that doesnt' do anything or because we don't want the IO
-- action that's made up from a do block to have the result value of its last action, but we want it to have a different value, so we make a 
-- return that always has our desired type as a result and we put it at the end. 

-- putStr is like putStrLn but it doesn't put an extra new line at the end. 
-- putStr :: String -> IO ()

-- putStr is defined recursively with the help of putChar. 
-- putChar just prints out a char. 

-- Let's make putStr!
putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
	putChar x
	putStr' xs

-- print takes a value that derives show and prints it to the terminal. When we are using GHCI, it's using print to print things out to the
-- screen. print is basically putStrLn . show 

-- Created getchartest.hs
-- getChar is like getLine, but it only gets a character.
-- Due to buffering, it doesn't actually run the program until we press the enter key. But once we press it, it's like we're just entering 
-- things as we did. 

-- We can use Control.Monad with this as well to use the 'when' function. Imported Control.Monad at the top. This is interesting because in a
-- 'do' block, it looks like a control flow statement, but it's actually a normal function. It takes a boolean value and an IO action. If
-- True, then it does the IO action, otherwise it return ()

-- Created getchartestmonad.hs
-- So using 'when' is good for encapsulating the 'if this then IO action else return ()'

-- Created sequence.hs
-- sequence takes a list of IO actions and then calls those IO actions one after another. 
-- sequence :: [IO a] -> IO [a]
{-
main = do
	a <- getLine
	b <- getLine
	c <- getLine
	print [a,b,c]

is exactly the same as doing

main = do
	rs <- sequence [getLine, getLine, getLine]
	print rs

-}

-- mapM and mapM_ are two functions that do mapping over lists. Because mapping an IO action over a list of so common, these two funcitons were 
-- created. mapM takes a function and a list, maps the function over the list, and then sequences it. 
-- mapM_ takes a function and a list and maps the function over the list, but gets rid of the results when we sequence it. We usually use mapM_
-- when we don't care about the result our sequence has. 

-- Created capslocked.hs
-- The forever function takes an IO action and returns that IO action over and over again forever. 