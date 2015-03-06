import Data.Char

main = do
	putStrLn "What's your first name?"
	fName <- getLine
	putStrLn "What's your last name?"
	lName <- getLine
	let 
		firstToUpper = map toUpper fName
		lastToUpper = map toUpper lName
	putStrLn $ "Hey " ++ firstToUpper ++ " " ++ lastToUpper ++ ", how are you?"