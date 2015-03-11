main = do
	interact shorterThan
	--contents <- getContents
	--putStr (shorterThan contents)

shorterThan :: String -> String 
shorterThan cont =
	let 
		allLines = lines cont
		shortLines = filter (\line -> length line < 10) allLines
		result = unlines shortLines
	in
		result