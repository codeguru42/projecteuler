-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 129

import Data.List (nub)

pascalsTriangle = iterate (\row -> zipWith (+) (0:row) (row ++ [0])) [1]

main = print . nub . concat $ take 51 pascalsTriangle
