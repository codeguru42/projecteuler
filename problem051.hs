-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 51

import ProjectEuler(isPrime)

replace :: String -> Char -> String
replace s d = map (\c -> if c == '*' then d else c) s

replaceAll :: String -> [Char] -> [String]
replaceAll s = map (replace s)

candidates :: String -> [Integer]
candidates s
  | head s == '*' = let ss = rest
                      in map read ss
  | otherwise = let ss = replace s '0' : rest
                  in map read ss
  where rest = replaceAll s ['1'..'9']

primeCandidates :: String -> [Integer]
primeCandidates s = filter isPrime (candidates s)

main =  do
  let s = "*3"
  let s' = "56**3"
  print (primeCandidates s)
  print (primeCandidates s')
