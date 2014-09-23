-- ([Typeclass] [var]) says that [var] must be in a certain typeclass in order to proceed. You can also have more than one restriction. (Int a, Show a, Read a)
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


-- PATTERN MATCHING --
-- When you call sayMe, it goes down the list of functions and specific arguments. You need to be careful 
-- in that you make sure that you have a catch for every input that's possible so that something does get returned.
-- The last function declaration should catch anything and everything. 
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5."


-- It can fail if you have a pattern that can't be matched. We don't have a catch-all at the end here. 
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"


-- Function to add together vectors
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Functions to get the 1st, 2nd, and 3rd value out of a tripple. 
first :: (a, a, a) -> a
first (a, _, _) = a
second :: (a, a, a) -> a
second (_, a, _) = a
third :: (a, a, a) -> a
third (_, _, a) = a


-- Head function to find the first value of a list. We use the _ to say that we don't really care about the value in that place.
head' :: [a] -> [a]
head' [] = error "Can't call head on an empty list."
head' (x:_:z:_) = [z, x]
head' (x:_) = [x]

-- Function to tell us something about a list that is passed. We use x:y:z because that says you're getting the first, second, and third elements from a list
-- that is passed. We could also say [x,y], but that would prevent us from using a catch-all at the end because we can't write an infinite amount of lists.
-- [] tells us that we are looking at an empty list. 
tell :: (Show a) => [a] -> String
tell [] = "This is an empty list."
tell (x:[]) = "This list has one item: " ++ show x ++ "."
tell (x:y:[]) = "This list has two items: " ++ show x ++ " and " ++ show y ++ "."
tell (x:y:_) = "This list has more than two items. The first two are " ++ show x ++ " and " ++ show y ++ "."



-- RECURSION --

-- Here, you're calling the factorial funciton, but you call it back onto itself until it gets to factorial 0, where it goes to 1, because we have that defined as 1. 
-- If we didn't define factorial 0 = 1, then the function would never terminate. 
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


-- Length function. Takes a list of some sort and gives a number back. Takes the first element from the list and calls the length' function on the rest of it recursively.
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:x) = 1 + length' x