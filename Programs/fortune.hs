main = do
	putStrLn "What's your name?"
	name <- getLine
	putStrLn $ "Read this carefully, " ++ name ++ ", for this is your future: " ++ getFortune name

getFortune :: String -> String
getFortune name = name ++ " will eat a biscuit."