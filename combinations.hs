-- Combinations

comb :: [a] -> Integer -> [[a]]
comb _ 0 = [[]]
comb xs@(x:xs') n
  | fromIntegral (length xs) == n = [xs]
  | otherwise = (map (x:) (comb xs' (n - 1))) ++ (comb xs' n)
  
main = do
  print (comb [1..3] 2)
  print (comb [1..5] 2)
  print (comb [1..10] 3)