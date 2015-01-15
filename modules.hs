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