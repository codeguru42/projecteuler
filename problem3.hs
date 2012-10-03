-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 3

isPrime :: Integer -> Bool
isPrime p = (divisors p) == [1, p]

divisors :: Integer -> [Integer]
divisors n = [d | d <- [1..n], n `mod` d == 0]

main = print (head (reverse (filter isPrime (divisors n))))
	where n = 600851475143
