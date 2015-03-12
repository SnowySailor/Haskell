import System.IO
import System.Directory
import Data.List

main = do
	handle <- openFile "todo.txt" ReadMode
	(tempName, tempHandle) <- openTempFile "." "temp"
	contents <- hGetContents handle
	let 
		todoTasks = lines contents
		numberedLines = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
	putStrLn "These are your TODO items:"
	putStrLn $ unlines numberedLines
	putStrLn "Which number would you like to delete?"
	numberString <- getLine
	let 
		number = read numberString
		newTodoItems = delete (todoTasks !! number) todoTasks
	hPutStr tempHandle $ unlines newTodoItems
	hClose handle
	hClose tempHandle
	removeFile "todo.txt"
	renameFile tempName "todo.txt"