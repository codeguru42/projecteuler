-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 69

countIf :: (Integral b) => [a] -> (a -> Bool) -> b
countIf [] p = 0
countIf (x:xs) p
  | p x = 1 + countIf xs p
  | otherwise = countIf xs p

isRelativelyPrime :: (Integral a) => a -> a -> Bool
isRelativelyPrime m n = gcd m n == 1

phi :: (Integral a, Integral b) => a -> b
phi n = countIf [1..(n - 1)] (isRelativelyPrime n)

maxRatio :: (Int, Int, Double) -> (Int, Int, Double) -> (Int, Int, Double)
maxRatio t1@(_, _, x) t2@(_, _, y)
  | x > y = t1
  | otherwise = t2

main = print 
         (foldl
           maxRatio
           (0, 0, 0.0)
           [(n, phi n, ratio) | n <- [2..1000000], let ratio = fromIntegral n / (fromIntegral (phi n))])
		   
-- This is my updated version which uses the ProjecEuler module which I lost on my broken USB drive ;-(
-- Problem 69

import ProjectEuler

phi :: Integer -> Integer
phi n = n * product [p - 1 | p <- primeDivisors n] `div` product [p | p <- primeDivisors n]

maxRatio :: (Integer, Integer, Double) -> (Integer, Integer, Double) -> (Integer, Integer, Double)
maxRatio t1@(_, _, x) t2@(_, _, y)
  | x > y = t1
  | otherwise = t2

main = print (foldl
                maxRatio
                (0, 0, 0.0)
                [(n, phi n, ratio) | n <- [2..max], let ratio = fromIntegral n / (fromIntegral (phi n))]
              )
    where max = 1000000
