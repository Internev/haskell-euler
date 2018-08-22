-- Problem 1. Multiples to limit.
-- Generalised to any to divisors and any limit.
prob1 :: Int -> Int -> Int -> Int
prob1 x y limit = sum [a | a <- [1..limit], a `mod` x == 0 || a `mod` y == 0]

-- Problem 2. Even Fibonacci numbers.
-- Using fancy fib generator, any limit.
prob2 :: Int -> Int
prob2 limit = sum [a | a <- takeWhile (<= limit) fibs, even a]
  where
    fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Problem 3. Largest Prime Factor.
-- Biggest hurdle here was the typing for the sqrt bit :S
prob3 num = foldr (\p acc -> if num `mod` p == 0 && p > acc then p else acc) 0 ps
  where
    ps = primes n
      where
        n = floor (sqrt (fromIntegral num))
        primes limit = 2 : primes'
          where isPrime (p:ps) n = p*p > n || n `rem` p /= 0 && isPrime ps n
                primes' = 3 : filter (isPrime primes') [5, 7 .. limit]

-- Problem 4. Largest Palindrome Product.
prob4 = foldr (\n acc -> if n == reversed n && n > acc then n else acc) 0 prods
  where
    reversed :: Integer -> Integer
    reversed = read . reverse . show
    prods = [x * y | x <- [900..999], y <- [900..999]]
