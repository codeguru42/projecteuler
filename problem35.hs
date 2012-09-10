-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 35

import ProjectEuler

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

rotateAll :: [a] -> [[a]]
rotateAll xs = take (length xs) (iterate rotate xs)

digitsToInteger :: [Integer] -> Integer
digitsToInteger ns = sum (zipWith (\n p -> n * 10 ^ p) ns [0..])

main = do
    print (length (filter (==True) (map (all isPrime) (map (map digitsToInteger) (map rotateAll (map digits (takeWhile (<n) primes)))))))
    where n = 1000000
