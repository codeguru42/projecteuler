-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 381

fact :: Integer -> Integer
fact n = product [1..n]

s :: Integer -> Integer
s p = (sum (map fact (map (p-) [1..5]))) `mod` p

main = do
  print (s 7)
  print (sum (map s [5..99]))
--  print (sum (map s [5..10^8-1]))