--HIGHER ORDER FUNCTIONS--

-- CURRIES FUNCTIONS --

-- Haskell functions only take one parameter. It sounds a little weird, but it kind of makes sense. 
-- If you call "max 4 5", it's the same as "(max 4) 5". Doing max 4 5 creates a function that takes a parameter and returns either 4 or that
-- parameter, depending on which is bigger. Then, 5 is applied to that function and it gives us our result. 