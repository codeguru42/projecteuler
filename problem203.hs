-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 203

import Data.List (nub)
import ProjectEuler

pascalsTriangle = iterate (\row -> zipWith (+) (0:row) (row ++ [0])) [1]

isSquareFree n = length (filter (\p -> n `mod` (p*p) == 0) ps) == 0
    where ps = takeWhile (\p -> p*p <= n) primes
    
main = do 
    let xs = filter isSquareFree . nub . concat $ take 51 pascalsTriangle
    print $ sum xs
