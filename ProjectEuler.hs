-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module ProjectEuler
( isPrime,
  divisors,
  divides,
  primeDivisors,
  primes
) where

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = (filter  (divides n) (takeWhile (\d -> d * d <= n) [1..n])) == [1]

-- All divisors of a number 
divisors :: Integer -> [Integer] 
divisors 1 = [1] 
divisors n = firstHalf ++ secondHalf 
    where firstHalf = filter (divides n) (candidates n) 
          secondHalf = filter (\d -> n `div` d /= d) (map (n `div`) (reverse firstHalf)) 
          candidates n = takeWhile (\d -> d * d <= n) [1..n]  

divides :: Integer -> Integer -> Bool
divides n = (==0) . (n `mod`)

primeDivisors :: Integer -> [Integer]
primeDivisors n = filter isPrime (divisors n)

primes :: [Integer]
primes = primes' [2..]

primes' :: [Integer] -> [Integer]
primes' [] = []
primes' (p:ps) = p:(primes' [p' | p' <- ps, not (p' `isMultiple` p)])

a `isMultiple` b = (a `mod` b) == 0
