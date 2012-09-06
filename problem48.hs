-- Problem 48

main = print (sum [a ^ a | a <- [1..1000]] `mod` (10 ^ 10))