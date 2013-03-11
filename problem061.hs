-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 61

discriminant :: Integer -> Integer -> Integer -> Integer
discriminant a b c = b ^ 2 - 4 * a * c

isQuadraticSolutionInteger :: Integer -> Integer -> Integer -> Bool
isQuadraticSolutionInteger a b c =  (d >= 0) && (isSquare d)
                                    && round (-b + sqrt (fromIntegral d))
                                    `mod` (2 * a) == 0
  where d = discriminant a b c

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

isTriangle :: Integer -> Bool
isTriangle k = isQuadraticSolutionInteger 1 1 (-2 * k)

square :: Integer -> Integer
square n = n ^ 2

isSquare :: Integer -> Bool
isSquare k = (round .  sqrt . fromInteger $ k) ^ 2 == k

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

isPentagonal :: Integer -> Bool
isPentagonal k = isQuadraticSolutionInteger 3 (-1) (-2 * k)

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

isHexagonal :: Integer -> Bool
isHexagonal k = isQuadraticSolutionInteger 2 (-1) (- k)

heptagonal :: Integer -> Integer
heptagonal n = n * (5 * n - 3) `div` 2

isHeptagonal :: Integer -> Bool
isHeptagonal k = isQuadraticSolutionInteger 5 (-3) (-2 * k)

octagonal :: Integer -> Integer
octagonal n = n * (3 * n - 2)

isOctagonal :: Integer -> Bool
isOctagonal k = isQuadraticSolutionInteger 3 (-2) (-k)

main = do
  let ns = [1..20]
  let fs = [(isTriangle, triangle), (isSquare, square),
            (isPentagonal, pentagonal), (isHexagonal, hexagonal),
            (isHeptagonal, heptagonal), (isOctagonal, octagonal)]
  print (map (\(f, g) -> (map f (map g ns))) fs)
