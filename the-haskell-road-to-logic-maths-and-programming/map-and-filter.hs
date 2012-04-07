-- Exercise 1.20
-- Function 'lengths' takes a list of lists and returns a list
-- of the corresponding list lengths.
lengths :: [[a]] -> [Int]
lengths a = map (length) a

-- Exercise 1.21
-- Write a function that gives the sum of a list of lists lengths.
sumLengths :: [[a]] -> Int
sumLengths = sum . lengths
-- This actually isn't possible with map so the exercise was wrong... :/
