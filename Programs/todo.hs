import System.IO
import Data.List
import Data.Char as C
import System.Directory
import System.Environment

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add),
			("remove", remove),
			("view", view)]

main = do
	(command:args) <- getArgs
	let (Just action) = lookup (map C.toLower command) dispatch
	action args