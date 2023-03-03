-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 56

import ProjectEuler (digits)

digitSum :: Integer -> Integer
digitSum = sum . digits
  
main = print (maximum (map digitSum ns))
    where max = 100
          ns = [(a ^ b) | a <- [1..max], b <- [1..max]]
