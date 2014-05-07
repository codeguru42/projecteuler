-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 81

import Control.Applicative ((<$>))
import Data.List.Split (splitOn)

toTriangle :: [[Int]] -> [[Int]]
toTriangle xss = [[t i j | i <- [0..j]] | j <- [0..m-1]]
    where m = length xss
          t i j = xss !! abs (i - j) !! i

minSumPath :: [[Int]] -> Int -> Int -> Int
minSumPath xss 0 0 = xss !! 0 !! 0
minSumPath xss i 0 = xss !! i !! 0 + minSumPath xss (i-1) 0
minSumPath xss 0 j = xss !! 0 !! j + minSumPath xss 0 (j-1)
minSumPath xss i j = min (x + minSumPath xss (i-1)  j   )
                         (x + minSumPath xss  i    (j-1))
    where x = xss !! i !! j
          max = length xss

main = do
    contents <- readFile "input/matrix2.txt"
    let xss = map (read <$>) . map (splitOn ",") $ lines contents :: [[Int]]
    mapM_ print xss
    print $ length xss
    print . length $ last xss
    print $ minSumPath xss (n-1) (n-1)
        where n = 5
