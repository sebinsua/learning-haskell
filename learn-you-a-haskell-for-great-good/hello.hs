#!/usr/bin/env runhaskell
import Data.Char

main :: IO()
main = do
    putStrLn "Hello, what's your first name?"
    firstName <- getLine
    putStrLn "And, what's your second name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName  = map toUpper lastName
    putStrLn ("Hello, " ++ bigFirstName ++ " " ++ bigLastName ++ "!")
