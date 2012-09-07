-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 56

digitSum n
  | n < 0 = error "sumDigits does not accept negative numbers"
  | n < 10 = n
  | otherwise = (n `mod` 10) + digitSum (n `div` 10)
  
main = print (maximum [digitSum (a ^ b) | a <- [1..100], b <- [1..100]])