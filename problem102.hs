-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 102

import Char(digitToInt)
import ProjectEuler(split)

type Point = (Double, Double)

data Triangle = Triangle Point Point Point

stringToInteger :: String -> Integer
stringToInteger s = fromIntegral 
                    $ sum 
                    $ zipWith (*) (map (10^) [0..]) 
                    $ map digitToInt 
                    $ reverse s

--contains :: Triangle -> Point -> Boolean
--contains t p = 

main = do
  contents <- readFile "input/triangles.txt"
  let triangles = map (split ',') (lines contents)
  print triangles
