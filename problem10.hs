-- Problem 10

primes :: [Int]
primes = primes' [2..]

primes' :: [Int] -> [Int]
primes' [] = []
primes' (p:ps) = p:(primes' [p' | p' <- ps, not (p' `isMultiple` p)])

a `isMultiple` b = (a `mod` b) == 0

main = print (sum (primes' [2..100000]))