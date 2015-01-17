-- MODULES --

-- LOADING MODULES -- 

-- When importing modules, import them before degining any functions and each on thier own line. 
import Data.List
import qualified Data.Map as M
-- Data.List contains useful functions for dealing with lists. 
-- One function in the Data.list moduel is the nub function, which takes a list and returns a list that has any duplicate elements removed.
-- Function to find the length of a list excluding duplicate elements.
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- You can also add modules into your current GHCI session with :m + <module name> <module name> <module name>, etc.
-- If you only need a few functions from a module, you can say 
-- ::: import <module name> (function, function, function)
-- So if we only needed the nub and sort functions, we could do 'import Data.List (nub, sort)'
-- We can also EXCLUDE certain functions when loading a module by using 'hiding'
-- So if we didn't want to have the nub function because it's a part of another module, we could import Data.List like
-- ::: 'import Data.List hiding (nub)'

-- Another way to import functions from a certian module that may interfere with the functions from another module is to use 'qualified'
-- Using 'qualified' means that we NEED to call a function like <module name>.<function name>, so like 'Data.List.nub' instead of just 'nub'
-- We'd import the Data.List module like 
-- ::: 'import qualified Data.Map'
-- We can simplify the name so we don't have to type Data.List every time by using 'as' in our imports. So we can say like 'D.nub', which would
-- be the same as 'Data.List.nub.' We do this by importing like
-- ::: 'import qualified Data.List as D'

-- DATA.LIST --
-- Data.List is all about lists, and it's got functions like map and filter in it. We don't need to qualify the import becuase it's got no
-- functions that clash with Prelude. 
-- intersperse takes an element and a list and puts that element in between each element of the list
inter :: String
inter = intersperse '.' "MONKEY"

-- intercalate takes a list and a list of lists, then puts the list in between each of the lists in the list of lists and then flattens it all
interCalc :: [Int]
interCalc = intercalate [0,0,0] [[1,2,3], [4,5,6], [7,8,9]]

-- transpose takes a list of lists and then "turns" them into a 2D matrix and returns the columns as lists of lists
transposeExample :: [[Int]]
transposeExample = transpose [[1,2,3],[4,5,6],[7,8,9]]
-- If we had the polynomials 3x^2 + 5x + 9, 10x^3 + 9 and 8x^3 + 5x^2 + x - 1, and we wanted to sum them all up, we can use transpose
transposePoly :: [Int]
transposePoly = map sum $ transpose [[0,3,5,9], [10,0,0,9], [8,5,1,-1]]

-- concat flstens a list of lists into a list of elements
concatTest :: [Char]
concatTest = concat ["hello", " ","there"]

-- concatMap is the same thing as mapping a function over a list and then flattening that list. 
concatMapTest :: [Int]
concatMapTest = concatMap (replicate 4) [1..3]

-- 'and' takes a list of boolean values and returns true only if all the elements of the list are True. 
andTest :: Bool
andTest = and $ map (<4) [1..3]
-- This will return False because there is an element that isn't less than three. 
andTestF :: Bool
andTestF = and $ map (<3) [1..3]

-- 'or' is like 'and', but it returns True if ANY of the values in the list are True.
orTest :: Bool
orTest = or $ map (<3) [2..50]

-- 'any' takes a predicate and a list and returns True if any of the values in the list satisfy the predicate.
anyTest :: Bool
anyTest = any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"

-- 'all' takes a predicate and a list and returns True only if ALL of the values in the list satisfy the predicate
allTest :: Bool
allTest = all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"

-- iterate takes a function and a starting value and iterates that function over the starting values. Returns an infinite list.
iterateTest :: [Int]
iterateTest = take 10 $ iterate (*2) 1

-- splitAt takes a number and a list and it splits the list at the position that the number represents
splitAtTest :: ([Char], [Char])
splitAtTest = splitAt 5 "hellothere"

-- takeWhile takes a predicate and a list and takes values from the list while the predicate is true. When it's no longer true, the function stops
takeWhile' :: [Int]
takeWhile' = takeWhile (>3) [6,5,4,3,2,1,3,2,5,7,8,2,6,4]
-- Function to determine the sum of all cubes that are less than 10,000.
sumCubes :: Int
sumCubes = sum $ takeWhile (<10000) $ map (^3) [1..]

-- dropWhile is like takeWhile, but it drops the elements from a list while the predicate is true.
dropWhile' :: [Int]
dropWhile' = dropWhile (<3) [1..10]
dropWhile'' :: [Char]
dropWhile'' = dropWhile (/=' ') "Hello there."

-- Example that shows how we can use dropWhile in order to determine when an event occours given a few data points.
stocks :: (Double, Int, Int, Int) 
stocks = head (dropWhile (\(val,y,m,d) -> val < 1000) [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)])

-- span is like takeWhile, but it returns a double of lists. One is what takeWhile would return, and the other is what it would leave behind.
spanExample :: [Char]
spanExample = 
	let 
		(fw, rest) = span (/=' ') "This is a sentence."
	in
		"First word: " ++ fw ++ ". Rest: " ++ rest

-- break will break a list in two and return a double is lists when the predicate is first met somewhat like span.
breakOn :: ([Int], [Int])
breakOn = break (==4) [1..10]

-- sort just sorts a list. The elements of the list must e of the Ord typeclass because they need to be able to be ordered.
sort' :: [Int]
sort' = sort [1,5,1,3,6,1,88,2,6,4,23]

-- group takes a list and returns a list of lists. Each of the sublists contains the same value. So [1,1,2,3,3,3] -> [[1,1],[2],[3,3,3]]
-- however, the elements must be adjecent to each other. 
group' :: [[Char]]
group' = group "Hello there. I am a list."
-- if we sort a list before we group it, we can see how many of each element are in a list
howMany :: [(Int, Int)]
howMany = map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]

-- inits is like init, but it applies recursively to the list that it's given. Returns a list of lists. 
initsTest :: [[Char]]
initsTest = inits "this is a list"

-- tails is just like inits, but it works backwards. Starts with the whole list and then gradually reduces the length.
tailsTest :: [[Char]]
tailsTest = tails "this is a list"

-- function to search a list for a sublist
listSearch :: (Eq a) => [a] -> [a] -> Bool
listSearch needle haystack = 
	let nlength = length needle
	in foldl (\acc x -> if take nlength x == needle then True else acc) False $ tails haystack

-- isInfixOf does the exact same thing as listSearch.
isInfixOfTest :: Bool
isInfixOfTest = isInfixOf "cat" "there's a cat in here"

-- isPrefixOf checks to see if your provided list is at the beginning of the list you're searching through
isPrefixOfTest :: Bool
isPrefixOfTest = isPrefixOf "hey" "oh hey there!"

-- isSuffixOf does the same thing as isPrefixOf, but it checks to see if the list you're searching through ENDS with your value
isSuffixOfTest :: Bool
isSuffixOfTest = isSuffixOf "there!" "oh hey there!"

-- find takes a predicate and a list and returns a Maybe value. That is, either Nothing or Just _, where _ is the element you searched for.
findTest :: Maybe Char
findTest = find (=='a') "this is a cat"

-- Rework the stocks function from earlier so that in case the stocks never go over 1,000, we don't get an arror for calling head on
-- an empty list. 
stocks' :: Maybe (Double, Int, Int, Int)
stocks' = find (\(val, y, m, d) -> val > 1000) $ [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]