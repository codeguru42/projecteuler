-- isPrime and divisors

isPrime :: Integer -> Bool
isPrime p = (divisors p) == [1, p]

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n `mod` x == 0]

primeDivisors :: Integer -> [Integer]
primeDivisors n = [d | d <- divisors n, isPrime d]

main = do {
  print [x | x <- [1..100], isPrime x];
  print (primeDivisors 13195);
}