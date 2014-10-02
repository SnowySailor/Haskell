-- RECURSION --
-- Function to determine the maximum of a list.
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list."
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = maximum' xs