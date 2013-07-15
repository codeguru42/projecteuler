-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 385

import Control.Applicative (liftA2)

data Vertex = Vertex Double Double
              deriving (Eq)

data Triangle = Triangle Vertex Vertex Vertex
                deriving (Show)

data Ellipse = Ellipse Double Double
data Line = Line Vertex Vertex

class Shape s where
    area :: s -> Double

instance Shape Triangle where
    area (Triangle v1 v2 v3) = let a = distance v1 v2
                                   b = distance v2 v3
                                   c = distance v3 v1
                                   s = (a + b + c) / 2
                               in sqrt $ s * (s - a) * (s - b) * (s - c)

instance Shape Ellipse where
    area (Ellipse a b) = pi * a * b

instance Show Vertex where
    show (Vertex x y) = ('(':show x) ++ (',':show y) ++ ")"

distance :: Vertex -> Vertex -> Double
distance (Vertex x1 y1) (Vertex x2 y2)
    = sqrt $ (x2 - x1)^2 + (y2 - y1)^2

focus :: Ellipse -> Double
focus (Ellipse a b) = sqrt $ a^2 - b^2

slope :: Line -> Double
slope (Line (Vertex x1 y1) (Vertex x2 y2)) = (y2 - y1) / (x2 - x1)

yIntercept :: Line -> Double
yIntercept l@(Line (Vertex x y) _) = let m = slope l
                                     in y - m*x

main = do
    let n = 8
    let sqrt13 = sqrt 13
    let ns = [-n..n]
    let vs = liftA2 Vertex ns ns
    let ts = [Triangle v1 v2 v3 | v1 <- vs,
                                  v2 <- dropWhile (/= v1) vs,
                                  v3 <- dropWhile (/= v2) vs]
    mapM_ print ts
