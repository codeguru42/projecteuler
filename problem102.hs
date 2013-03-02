-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 102

import Char(digitToInt)
import ProjectEuler(split)

type Point = (Double, Double)

x :: Point -> Double
x = fst

y :: Point -> Double
y = snd

data Triangle = Triangle {
  a :: Point,
  b :: Point,
  c :: Point
  } deriving (Show)

stringToInteger :: String -> Integer
stringToInteger ('-':xs) = -1 * stringToInteger xs
stringToInteger s = fromIntegral 
                    $ sum 
                    $ zipWith (*) (map (10^) [0..]) 
                    $ map digitToInt 
                    $ reverse s

makePoints :: [Integer] -> [Point]
makePoints [] = []
makePoints (x:y:ps) = (fromInteger x, fromInteger y) : makePoints ps

--contains :: Triangle -> Point -> Boolean
--contains t p = 

main = do
  contents <- readFile "input/triangles.txt"
  let coords = map (map stringToInteger) (map (split ',') (lines contents))
  let triangles = map (\(a:b:c:[]) -> Triangle a b c) (map makePoints coords)
  print triangles
