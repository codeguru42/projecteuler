-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 124

import Data.List (sortBy)

import ProjectEuler

rad = product . primeDivisors

e n k = fst $ sorted !! k
    where sorted = sortBy (\(x1, y1) (x2, y2) -> if y1 == y2
                                                 then x1 `compare` x2
                                                 else y1 `compare` y2)
                    . zip [1..]
                    $ map rad [1..n]

main = print $ e 100000 (10000 - 1)
