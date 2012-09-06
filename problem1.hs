-- Problem 1

sumN n = n * (n + 1) `div` 2
sumMultiples n m = m * sumN (n `div` m)

main = do {
    print (sumMultiples 999 3);
    print (sumMultiples 999 5);
    print (sumMultiples 999 15);
    print ((sumMultiples 999 3) + (sumMultiples 999 5) - (sumMultiples 999 15));
  }