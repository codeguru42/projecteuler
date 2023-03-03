-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://sam.zoy.org/wtfpl/COPYING for more details.

-- Problem 89

fromRomanNumerals :: String -> Int
fromRomanNumerals "" = 0
fromRomanNumerals ('I':'V':ns) =    4 + fromRomanNumerals ns
fromRomanNumerals ('I':'X':ns) =    9 + fromRomanNumerals ns
fromRomanNumerals ('V':ns)     =    5 + fromRomanNumerals ns
fromRomanNumerals ('I':ns)     =    1 + fromRomanNumerals ns
fromRomanNumerals ('X':'L':ns) =   40 + fromRomanNumerals ns
fromRomanNumerals ('X':'C':ns) =   90 + fromRomanNumerals ns
fromRomanNumerals ('X':ns)     =   10 + fromRomanNumerals ns
fromRomanNumerals ('L':ns)     =   50 + fromRomanNumerals ns
fromRomanNumerals ('C':'D':ns) =  400 + fromRomanNumerals ns
fromRomanNumerals ('C':'M':ns) =  400 + fromRomanNumerals ns
fromRomanNumerals ('C':ns)     =  100 + fromRomanNumerals ns
fromRomanNumerals ('D':ns)     =  500 + fromRomanNumerals ns
fromRomanNumerals ('M':ns)     = 1000 + fromRomanNumerals ns

toMinimalRomanNumerals :: Int -> String
toMinimalRomanNumerals n = ths ++ hs ++ ts ++ os
    where thousands = n `div` 1000
          hundreds  = n `mod` 1000 `div` 100
          tens      = n `mod` 100  `div` 10
          ones      = n `mod` 10
          ths       = replicate thousands 'M'
          hs        = if hundreds == 9
                      then "CM"
                      else if hundreds == 4
                      then "CD"
                      else if hundreds >= 5
                      then 'D' : replicate (hundreds - 5) 'C'
                      else replicate hundreds 'C'
          ts        = if tens == 9
                      then "XC"
                      else if tens == 4
                      then "XL"
                      else if tens >= 5
                      then 'L' : replicate (tens - 5) 'X'
                      else replicate tens 'X'
          os        = if ones == 9
                      then "IX"
                      else if ones == 4
                      then "IV"
                      else if ones >= 5
                      then 'V' : replicate (ones - 5) 'I'
                      else replicate ones 'I'

main = do
    content <- readFile "../input/roman.txt"
    let ls = lines content
    let ns = map fromRomanNumerals ls
    let rs = map toMinimalRomanNumerals ns
    let diffs = zipWith (\x y -> length x - length y) ls rs
    print $ sum diffs
