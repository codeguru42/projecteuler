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

main = do
    contents <- readFile "input/matrix.txt"
    let xss = map (read <$>) . map (splitOn ",") $ lines contents :: [[Int]]
    mapM_ print $ toTriangle xss
