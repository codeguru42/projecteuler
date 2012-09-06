-- Prime Generator

primes :: [Integer]
primes = primes' [2..]

primes' :: [Integer] -> [Integer]
primes' [] = []
primes' (p:ps) = p:(primes' [p' | p' <- ps, not (p' `isMultiple` p)])

a `isMultiple` b = (a `mod` b) == 0

main = print (sum (primes' [2..100000]))