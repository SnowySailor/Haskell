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