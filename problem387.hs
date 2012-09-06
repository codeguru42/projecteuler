-- Problem 387

isPrime :: Integer -> Bool
isPrime p = (divisors p) == [1, p]
 
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n `mod` x == 0]

digits :: Integer -> [Integer]
digits n
  | n < 0 = digits (abs n)
  | n < 10 = [n]
  | otherwise = (digits (n `div` 10)) ++ [n `mod` 10]
  
isHarshad :: Integer -> Bool
isHarshad n = n `mod` (sum (digits n)) == 0

isRightTruncHarshad :: Integer -> Bool
isRightTruncHarshad n
  | n < 10 = True
  | otherwise = isHarshad n && isRightTruncHarshad (n `div` 10)

isStrongHarshad :: Integer -> Bool
isStrongHarshad n = isHarshad n && isPrime (n `div` (sum (digits n)))

isStrongRightTruncHarshadPrime :: Integer -> Bool
isStrongRightTruncHarshadPrime n
  | n < 10 = False
  | otherwise = isPrime n && isStrongHarshad n' && isRightTruncHarshad n'
  where n' = n `div` 10
  
main = print (sum (map (\n -> if isStrongRightTruncHarshadPrime n then n else 0) [1..max]))
  where max = 10000