-- Integral contains only Integer and Int types.
-- Integral is being used as a typeclass in this example.
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- a is a generic type.
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
-- If this was at the top then the pattern-matching would
-- stop the rest of the other patterns from matching.
sayMe x = "Not between 1 and 5"

-- Recursive factorial function
factorial :: (Integral a) => a -> a
-- When this matches it stops recursing... :)
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

tell :: (Show a) => [a] -> String
tell [] = "This list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list has more than two elements. The first two elements are: " ++ show (x, y)

capital :: String -> String
capital "" = "An empty string, whoops!"
-- all is a reference to the whole (x:xs)
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, bro."
    | bmi <= normal = "You're ...normal."
    | bmi <= weight = "You're fat."
-- Otherwise always returns True
    | otherwise   = "Oh dear..."
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Let bindings are similar to where bindings.
-- However let is more local and does not span across 'guards'.
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * (r ^ 2)
-- The let bindings above are specifically accessible in the in expression.
    in  sideArea + 2 * topArea

-- NOTE: where is a syntactic construction while let is an expression
-- so therefore it can be used practically anywhere.
-- They can even be used in a list comprehension next to the predicates.

-- Usage: case 'expression' of 'pattern'
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."


