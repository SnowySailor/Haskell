module Arith3Broken where

main :: IO ()
main = do
    print $ show $ 1 + 2
    putStrLn $ show 10
    print (negate (-1))
    print ((+) 0 blah)
    where blah = negate 1

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst . g $ f x

co :: (b -> c) -> (a -> b) -> (a -> c)
co a b = a . b

a :: (a -> c) -> a -> a
a f x = x

a' :: (a -> b) -> a -> b
a' f = f
