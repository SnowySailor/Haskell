import System.Environment
import qualified Data.ByteString.Lazy as B

main = do
	(file1:file2:_) <- getArgs
	copyFile file1 file2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
	contents <- B.readFile source
	B.writeFile dest contents