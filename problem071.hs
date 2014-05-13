-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 71

import Control.Applicative
import Data.List
import Data.Ratio

fractions d = map (%d) [1..d-1]

justLessThan f d = ((numerator f * d - 1) `div` denominator f) % d

main = print . last . sort $ map (justLessThan (3%7)) [2..d]
    where d = 1000000
