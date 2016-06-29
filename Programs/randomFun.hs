import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 $ randomRs ('a','Z') gen