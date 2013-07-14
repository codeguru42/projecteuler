-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 385

data Vertex = Vertex Double Double
data Triangle = Triangle Vertex Vertex Vertex

class Shape s where
    area :: s -> Double

instance Shape Triangle where
    area (Triangle v1 v2 v3) = let a = distance v1 v2
                                   b = distance v2 v3
                                   c = distance v3 v1
                                   s = (a + b + c) / 2
                               in sqrt $ s * (s - a) * (s - b) * (s - c)

distance :: Vertex -> Vertex -> Double
distance (Vertex x1 y1) (Vertex x2 y2)
    = sqrt $ (x2 - x1)^2 + (y2 - y1)^2
