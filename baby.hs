doubleMe x = x + x

-- doubleUs x y = x*2 + y*2
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

-- Lists are homogenous in Haskell.
lostNumbers = [4, 8, 15, 16, 23, 42]

-- Odd numbers greater than 10 are BANG!
-- Odd numbers less than 10 are BOOM!
-- Even numbers are thrown out of the list.
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

notTheseNumbers = [x | x <- [10..20], x /= 13, x /= 15, x /= 19]

productsOfLists xs ys = [ x*y | x <- xs, y <- ys]

removeNestedOddNumbers xxs = [ [x | x <- xs, even x ] | xs <- xxs]

triangles = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10]]

rightTriangles = [ (a, b, c) | (a,b,c) <- triangles, a^2 + b^2 == c^2]

rightTriangles' = [ (a, b, c) | (a, b, c) <- rightTriangles, a + b + c == 24]
