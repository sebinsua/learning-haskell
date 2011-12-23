-- Can be used for partial application.
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- Takes a function which takes two arguments and outputs c.
-- Applies this to two lists by applying the function between
-- corresponding elements.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
-- flip' f = g
--    where g x y = f y x
flip' f = \x y -> f y x

-- A more readable quicksort. Uses filter rather than list comprehensions.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted  = quicksort (filter (>x)  xs)
    in  smallerSorted ++ [x] ++ biggerSorted

-- The largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
-- Because we use head on the filtered list it doesn't matter
-- if the filtered list is finite or infinite.
-- The evaluation stops when the first adequate solution is found.
-- This is due to lazyness.
largestDivisible = head (filter p [100000,999999..])
    where p x = x `mod` 3829 == 0

-- For all starting numbers between 1 and 100, how many chains have
-- a length greater than 15?
collatzSequence :: (Integral a) => a -> [a]
collatzSequence 1 = [1]
collatzSequence n
    | even n = n:collatzSequence (n `div` 2)
    | odd  n = n:collatzSequence (n*3 + 1)
numLongChains :: Int
-- numLongChains = length (filter isLong (map collatzSequence [1..100]))
--    where isLong xs = length xs > 15
numLongChains = length (filter (\xs -> length xs > 15) (map collatzSequence [1..100))

listOfMultiplicationFunctions = map (*) [0..]
-- (listOfMultiplicationFunctions !! 5) 4) == 20

sum' :: (Num a) => [a] -> a
-- The lambda is the binary function.
-- 0 is the starting value.
-- xs is the list to be folded up.
-- First the accumulator (acc) is 0, but then the first element is added to it (x).
-- And so on until x is the last element and is added to the accumulator...
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0
-- The above works because of currying.

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- How many elements does it take for the sum of the roots of all ]
-- natural numbers to exceed 1000?
sqrtSums :: Int
-- scanl1 uses the left-most element as the starting value.
-- Scans monitor the progression of a function: reporting all intermediate
-- accumulator states in the form of a list.
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..))) + 1

-- Function association with right-associative $
-- This:
-- sum (3 + 4 + 9)
-- is the same as:
-- sum $ 3 + 4 + 9
-- But it also allows things like this:
-- map ($ 3) [(4+), (10*), (^2), sqrt]


-- Function composition can be used to make functions on the fly in a
-- cleaner way than through lambdas.
-- map (\x -> negate (abs x)) [5,4,3,2,1,0,-2]
-- map (negate . abs) [5,4,3,2,1,0,-2]

