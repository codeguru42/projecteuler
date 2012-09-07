-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 60

concatInts :: Integer -> Integer -> Integer
concatInts n m = n * (10 ^ countDigits m) + m

countDigits :: Integer -> Integer
countDigits n
  | abs n < 10 = 1
  | otherwise = 1 + countDigits (n `div` 10)

isPrime :: Integer -> Bool
isPrime p = (divisors p) == [1, p]
 
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n `mod` x == 0]

cross :: [a] -> [b] -> [(a, b)]
cross ms ns = [(m, n) | m <- ms, n <- ns]

comb :: [a] -> Integer -> [[a]]
comb _ 0 = [[]]
comb xs@(x:xs') n
  | fromIntegral (length xs) == n = [xs]
  | otherwise = (map (x:) (comb xs' (n - 1))) ++ (comb xs' n)

isConcatPrime :: Integer -> Integer -> Bool
isConcatPrime m n = isPrime (concatInts m n)

primes :: [Integer]
primes = primes' [2..]
 
primes' :: [Integer] -> [Integer]
primes' [] = []
primes' (p:ps) = p:(primes' [p' | p' <- ps, not (p' `isMultiple` p)])
 
a `isMultiple` b = (a `mod` b) == 0

main = print (map (flip comb 2) (comb (takeWhile (<maxPrime) primes) 5))
  where maxPrime = 10000
        concatPrimes (p:q:[]) = (isConcatPrime p q) && (isConcatPrime q p)