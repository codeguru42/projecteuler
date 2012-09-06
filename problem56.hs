-- Problem 56

digitSum n
  | n < 0 = error "sumDigits does not accept negative numbers"
  | n < 10 = n
  | otherwise = (n `mod` 10) + digitSum (n `div` 10)
  
main = print (maximum [digitSum (a ^ b) | a <- [1..100], b <- [1..100]])