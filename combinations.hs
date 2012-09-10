-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Combinations

comb :: [a] -> Integer -> [[a]]
comb _ 0 = [[]]
comb xs@(x:xs') n
  | fromIntegral (length xs) == n = [xs]
  | otherwise = (map (x:) (comb xs' (n - 1))) ++ (comb xs' n)
  
main = do
  print (comb [1..3] 2)
  print (comb [1..5] 2)
  print (comb [1..10] 3)
