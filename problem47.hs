-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 47

import Data.List
import ProjectEuler

main = do
    print (fst (head (filter (\(n, ds) -> (all (==consecutiveCount) ds))
          (zip ns (map (map length) (map (map primeDivisors) consecutives))))))
    where consecutiveCount = 4
          consecutive n start = take n [start..]
          consecutives = map (consecutive consecutiveCount) ns
          ns = [1..]
