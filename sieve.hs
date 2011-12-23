#!/usr/bin/env runhaskell

sieve :: (Integral a)=> [a] -> [a]
sieve ns=n : sieve ns'
    where
        n=head ns
        ns'=filter ((/=0) . flip rem n) ns

primes :: (Integral a)=> [a]
primes=sieve [2..]

main :: IO ()
main=do
    putStrLn (show (take 10 primes))
