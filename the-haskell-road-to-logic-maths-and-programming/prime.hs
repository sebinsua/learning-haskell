divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

-- Exercise 1.5
prime0 :: Integer -> Bool
prime0 n | n < 1      = error "Not a positive integer."
         | n == 1     = False
         | otherwise  = ld n == n

-- Exercise 1.6
-- The type declaration for rem would look like:
-- rem :: Integer -> Integer -> Integer
-- NOTE: This actually isn't true though because of a type scheme.
--       It would be sensible to change the type of divides to:
--       divides :: Integral a -> a -> a -> Bool

factors :: Integer -> [Integer]
factors n | n < 1      = error "Argument not positive."
          | n == 1     = []
          | otherwise  = p : factors (div n p) where p = ld n

-- Exercise 1.24
-- If you remove an argument and it's in the type then it is implied
-- and applied if it exists -- otherwise the function is partial.
