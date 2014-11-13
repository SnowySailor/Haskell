--HIGHER ORDER FUNCTIONS--

-- CURRIED FUNCTIONS --

-- Haskell functions only take one parameter. It sounds a little weird, but it kind of makes sense. 
-- If you call "max 4 5", it's the same as "(max 4) 5". Doing max 4 5 creates a function that takes a parameter and returns either 4 or that
-- parameter, depending on which is bigger. Then, 5 is applied to that function and it gives us our result. 
foo :: (Num a) => a -> a -> a
foo a b = 2 * a - 3 * b
-- If you call "foo 1" it will return a partially applied function. So it will reuturn "foo 1 b = 2 * 1 - 3 * b". 
-- let function = foo 1 (returns 'function 1 b = 2*1-3*b')
-- function 3 (reuturns -7 because function 1 3 = -7)
-- You could also say "let multTwoWithNine = multThree 9 in multThree x y z = x*y*z" then call "multTwoWithNine 2 3" and get 54 out of it. 
-- Then you could also say "let multWithEighteen = multTwoWithNine 2" so that multWithEighteen x = (((multThree 9) 2) x)

multThree :: (Floating a) => a -> a -> a -> a
multThree x y z = z*y*z
-- Function to compare 100 to a number
compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred = compare 100 -- compare 100 returns a function that takes one parameter. Basically "compare 100 x"

-- INFIX FUNCTIONS --
-- Creates a function that is partially applied, but can be used with an operator (*, +, -, /, %)
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- Function to check if a character is upper case
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Function that subtracts 4. You need to use the "subtract" function with it otherwise it thinks -4 is negative 4.
subtractFour :: (Num a) => a -> a
subtractFour = (subtract 4)

-- Functions that take functions as parameters! It then applies the function to the result of the function.
-- So if you type applyTwice (3*) 10, it does 3*(3*10) => 90
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- You can also say 'let functionName = applyTwice (3*)' so that you can type functionName 5 and get teh number 45.

--zipWith function. Takes function as parameter, then returns a zipped list.
-- So calling "zipWith' (*) [1,2,3] [1,2,3]" returns [1,4,9]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:xy) = f x y : zipWith' f xs xy

-- Simple function to flip the order of the parameters of a function.
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x


-- MAP AND FILTER --
-- These can both be done with list comprehensions, but doing them this way is easier to understand. 
-- Map function takes a function and a list and then applies that function to every element in the list.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Function to filter out unwanted values from a list.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x = x : filter' f xs
	| otherwise = filter' f xs

-- Quicksort function redefined using the filter function.
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
	let 
		smallerSorted = quicksort' (filter' (<=x) xs)
		largerSorted = quicksort' (filter' (x<) xs)
	in smallerSorted ++ [x] ++ largerSorted

-- Finds the largest number that is divisible by 3829.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
	where p x = x `mod` 3829 == 0

-- takeWhile takes a predicate and a list. It looks over the list and stops taking once it finds a place in the list where the predicate fails.
-- sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) will give us the sum of all odd squares that are less than 10,000. 
-- takeWhile (/=' ') "some string goes here" will give us the first word in a string. 