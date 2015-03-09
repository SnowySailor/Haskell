import Control.Monad
import Data.Char

main = forever $ do
	putStrLn "Give me some input:"
	c <- getLine
	putStrLn $ map toUpper c