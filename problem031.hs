-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 31

countChange :: Int -> [Int] -> Int
countChange 0 _ = 0
countChange _ [] = 0
countChange n (x:[]) = if n `mod` x == 0 then 1 else 0
countChange n (x:xs) = countChange n xs + countChange (n - x) (x:xs) + maybePlusOne
    where maybePlusOne = if n == x then 1 else 0

countChangeDP :: Int -> [Int] -> Int
countChangeDP n xs = countChangeDP' !! (l-1) !! (n)
    where l = length xs
          countChangeDP' = [[cc i j | i <- [0..n]] | j <- [0..l-1]]
          cc 0 _ = 0
          cc i 0 = if i `mod` head xs == 0 then 1 else 0
          cc i j = countChangeDP' !! (j - 1) !! i
                 + countChangeDP' !! j !! max 0 (i - (xs !! j))
                 + maybePlusOne
            where maybePlusOne = if i == xs !! j then 1 else 0

main = print $ countChangeDP n denoms
    where n = 200
          denoms = [1, 2, 5, 10, 20, 50, 100, 200]
