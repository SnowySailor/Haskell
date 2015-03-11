import System.IO

main = do
	todo <- getLine
	appendFile "todo.txt" $ todo ++ "\n"