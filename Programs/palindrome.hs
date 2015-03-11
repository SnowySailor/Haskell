main = interact palindrome

palindrome :: String -> String
palindrome contents = unlines . map (\line -> if isPal line then "palindrome" else "not a palindrome") $ lines contents
	where
		isPal xs = xs == reverse xs