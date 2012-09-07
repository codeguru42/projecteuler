-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 10

primes :: [Int]
primes = primes' [2..]

primes' :: [Int] -> [Int]
primes' [] = []
primes' (p:ps) = p:(primes' [p' | p' <- ps, not (p' `isMultiple` p)])

a `isMultiple` b = (a `mod` b) == 0

main = print (sum (primes' [2..100000]))