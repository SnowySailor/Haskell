-- ([Typeclass] [var]) says that [var] must be in a certain typeclass in order to proceed. You can also have more than one restriction. (Int a, Show a, Read a)
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


-- Pattern matching. When you call sayMe, it goes down the list of functions and specific arguments. You need to be careful 
-- in that you make sure that you have a catch for every input that's possible so that something does get returned.
-- The last function declaration should catch anything and everything. 
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5."

-- RECURSION --

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)