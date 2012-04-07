-- Exercise 1.9
-- A function that gives the maximum of a list of integers.
maxIntList :: Integral a => [a] -> a
maxIntList [] = error "Empty list."
maxIntList [x] = x
maxIntList (x:xs) = max x (maxIntList xs)

-- Exercise 1.10
-- Remove the first occurence of an integer m from a list of integers
-- or leave it unchanged it the integer does not exist in the list.
removeFst :: Integral a => [a] -> a -> [a]
removeFst [] m = []
removeFst (x:xs) m | x == m    = removeFst xs m
                   | otherwise = [x] ++ removeFst xs m
-- Yay. First bit of Haskell code that I've not had any help with!
-- Example 1.11 presumed different formal argument order. ugh...

-- Exercise 1.13
-- Write a function to count the number of occurences of a
-- character in a string.
count :: Char -> String -> Int
count x [] = 0
count x (c:cs) | x == c = 1 + count x cs
               | otherwise = count x cs

-- Exercise 1.14
-- Write function blowup, which incrementally repeats characters in a string.
blowup :: String -> String
blowup [] = []
blowup (c:cs) = blowup' 1 (c:cs)
blowup' n [] = []
blowup' n (c:cs) = replicate n c ++ blowup' (n + 1) cs
-- I'm sure this can be done in a better way, but I don't know the syntax.

-- Exercise 1.15
-- Sort a list of strings in alphabetical order
-- Another day...!
