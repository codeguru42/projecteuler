-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 92

import ProjectEuler

squareDigits :: Int -> Int
squareDigits = sum . map (\x -> x * x) . digits

allSquareDigits :: [Int]
allSquareDigits = map squareDigits [0..]

squareDigitsChain :: Int -> [Int]
squareDigitsChain n = n : squareDigitsChain (allSquareDigits !! n)

chainEnds :: [Int]
chainEnds = [c' n | n <- [1..]]
    where c'  1 = 1
          c' 89 = 89
          c'  n = chainEnds !! ((squareDigits  n) - 1)

main = print . sum . map (\x -> if x == 89 then 1 else 0) $ take n chainEnds
    where n = 10000000
