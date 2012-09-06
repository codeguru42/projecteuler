-- Problem 381

fact :: Integer -> Integer
fact n = product [1..n]

s :: Integer -> Integer
s p = (sum (map fact (map (p-) [1..5]))) `mod` p

main = do
  print (s 7)
  print (sum (map s [5..99]))
--  print (sum (map s [5..10^8-1]))