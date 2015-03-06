main = do --putStrLn "Hello, World!"
	putStrLn "Hello. What's your name?"
	name <- getLine
	putStrLn ("Hey, " ++ name ++ ", you rock!")