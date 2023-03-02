-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 61

import Data.List (nub)
import Control.Applicative ((<$>), (<*>))

discriminant :: Integer -> Integer -> Integer -> Integer
discriminant a b c = b ^ 2 - 4 * a * c

isQuadraticSolutionInteger :: Integer -> Integer -> Integer -> Bool
isQuadraticSolutionInteger a b c =  (d >= 0) && (isSquare d)
                                    && (-b + (round . sqrt . fromIntegral) d)
                                    `mod` (2 * a) == 0
                                      where d = discriminant a b c

triangle :: Integer -> Integer
triangle n = n * (n + 1) `div` 2

isTriangle :: Integer -> Bool
isTriangle k = isQuadraticSolutionInteger 1 1 (-2 * k)

square :: Integer -> Integer
square n = n ^ 2

isSquare :: Integer -> Bool
isSquare k = (round .  sqrt . fromInteger $ k) ^ 2 == k

pentagonal :: Integer -> Integer
pentagonal n = n * (3 * n - 1) `div` 2

isPentagonal :: Integer -> Bool
isPentagonal k = isQuadraticSolutionInteger 3 (-1) (-2 * k)

hexagonal :: Integer -> Integer
hexagonal n = n * (2 * n - 1)

isHexagonal :: Integer -> Bool
isHexagonal k = isQuadraticSolutionInteger 2 (-1) (- k)

heptagonal :: Integer -> Integer
heptagonal n = n * (5 * n - 3) `div` 2

isHeptagonal :: Integer -> Bool
isHeptagonal k = isQuadraticSolutionInteger 5 (-3) (-2 * k)

octagonal :: Integer -> Integer
octagonal n = n * (3 * n - 2)

isOctagonal :: Integer -> Bool
isOctagonal k = isQuadraticSolutionInteger 3 (-2) (-k)

takeFourDigits :: [Integer] -> [Integer]
takeFourDigits l = takeWhile (<10000) $ dropWhile (<1000) l

isPair :: Integer -> Integer -> Bool
isPair x y = x `mod` 100 == y `div` 100

fours = takeFourDigits <$> ($ [1..]) <$> map <$> fs
    where fs = [triangle, square, pentagonal, hexagonal, heptagonal, octagonal]

filterPairs :: Integer -> [Integer] -> [Integer]
filterPairs x xs = filter (isPair x) xs

continueChain :: [Integer] -> [Integer] -> [[Integer]]
continueChain []     xs = map (:[])   xs
continueChain (c:cs) xs = map (:c:cs) heads
    where heads = filterPairs c xs

findChains xs = iterate (concat . map (\as -> continueChain as xs)) [[]]

oneOfEach fs = any isTriangle as
            && any isSquare bs
            && any isPentagonal cs
            && any isHexagonal ds
            && any isHeptagonal es
            && any isOctagonal fs
    where --bs = filter (not . isTriangle) as
          as = filter (not . isSquare) bs
          bs = filter (not . isPentagonal) cs
          cs = filter (not . isHexagonal) ds
          ds = filter (not . isHeptagonal) es
          es = filter (not . isOctagonal) fs

main = do
    let chains = findChains xs !! 6
    let cycles = filter (\as -> isPair (head as) (last as)) chains
    let theOne = filter oneOfEach cycles
    mapM_ print theOne
    print . head . nub $ map sum theOne
    where xs = nub $ concat fours
