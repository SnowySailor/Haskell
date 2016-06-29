-- FUNCTIONALLY SOLVING PROBLEMS --

-- Reverse Polish Notation Calculator --
-- First think about the type delcaration .Tells us a lot about the function.
-- think back to how you did it by hand and see if you can do something similar
solveRPN :: (Num a, Read a, Fractional a, Floating a) => String -> a
solveRPN s = head . foldl foldingFunction [] $ words s
             where foldingFunction (x:y:ys) "+"    = (x + y):ys
                   foldingFunction (x:y:ys) "*"    = (x * y):ys
                   foldingFunction (x:y:ys) "-"    = (y - x):ys
                   foldingFunction (x:y:ys) "/"    = (y / x):ys
                   foldingFunction (x:y:ys) "^"    = (x ** y):ys
                   foldingFunction (x:xs) "ln"     = (log x):xs
                   foldingFunction xs "sum"        = [sum xs]
                   foldingFunction xs numberString = read numberString : xs