-- ([Typeclass] [var]) says that [var] must be in a certain typeclass in order to proceed.
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"