-- Problem 1, generalised to any to divisors and any limit.
prob1 :: Int -> Int -> Int -> Int
prob1 x y limit = sum [a | a <- [1..limit], a `mod` x == 0 || a `mod` y == 0]

-- Problem 2. Using fancy fib generator, any limit.
prob2 :: Int -> Int
prob2 limit = sum [a | a <- takeWhile (<= limit) fibs, even a]
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
