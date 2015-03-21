import Data.List
import System.Random
import Control.Monad(when)

main = do
	gen <- getStdGen
	askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
	let (randomNum, newGen) = randomR (1,10) gen :: (Int, StdGen)
	putStrLn "What number from 1 to 10 am I thinking of?"
	numberString <- getLine
	when(not $ null numberString) $ do
		let number = read numberString
		if randomNum == number 
			then putStrLn "You are correct!"
			else putStrLn $ "Nope. I was thinking of " ++ show randomNum
		askForNumber newGen