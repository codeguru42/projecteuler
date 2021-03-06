-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

module ProjectEuler
( isPrime,
  divisors,
  divides,
  primeDivisors,
  primes,
  wordScore,
  removeChar,
  digits,
  phi,
  count
) where

import Data.Char (ord)
import Data.List (sort, group)
import Data.List.Ordered (minus)
import qualified Data.Map as Map

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = (filter (divides n) (takeWhile (\d -> d * d <= n) [1..n])) == [1]

-- All divisors of a number 
divisors :: Integer -> [Integer] 
divisors 1 = [1] 
divisors n = firstHalf ++ secondHalf 
    where firstHalf = filter (divides n) (candidates n) 
          secondHalf = filter (\d -> n `div` d /= d) 
                       (map (n `div`) (reverse firstHalf)) 
          candidates n = takeWhile (\d -> d * d <= n) [1..n]  

divides :: Integer -> Integer -> Bool
divides n = (==0) . (n `mod`)

primeDivisors :: Integer -> [Integer]
primeDivisors n = filter (divides n) ps
  where ps = takeWhile (\p -> p * p <= n) primes

primes :: [Integer]
primes = sieve [2..]

sieve xs = sieve' xs Map.empty
  where
    sieve' []     table = []
    sieve' (x:xs) table =
      case Map.lookup x table of
        Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
        Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
        where
          reinsert table prime = Map.insertWith (++) (x+prime) [prime] table

charScore :: Char -> Integer
charScore c = fromIntegral (ord c - ord 'A' + 1)

wordScore :: String -> Integer
wordScore s = sum (map charScore s)

removeChar :: Char -> String -> String 
removeChar _ [] = [] 
removeChar ch (c:cs) 
    | c == ch   = removeChar ch cs 
    | otherwise = c:(removeChar ch cs) 

digits :: Integral a => a -> [a]
digits n 
    | abs n < 10 = [n]
    | otherwise = (n `mod` 10) : (digits (n `div` 10))

phi :: Integer -> Integer
phi n = foldl div n ps * product [p - 1 | p <- ps]
  where ps = primeDivisors n

count :: (Ord a) => [a] -> [(a, Int)]
count = (map $ \xs -> (head xs, length xs)) . group . sort
