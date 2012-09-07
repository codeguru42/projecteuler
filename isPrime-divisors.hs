-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- isPrime and divisors

isPrime :: Integer -> Bool
isPrime p = (divisors p) == [1, p]

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n `mod` x == 0]

primeDivisors :: Integer -> [Integer]
primeDivisors n = [d | d <- divisors n, isPrime d]

main = do {
  print [x | x <- [1..100], isPrime x];
  print (primeDivisors 13195);
}