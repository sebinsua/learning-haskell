#!/usr/bin/env runhaskell

greeting :: String
greeting="Goodbye " ++ "World!"

main :: IO()
main=do
    putStrLn greeting
