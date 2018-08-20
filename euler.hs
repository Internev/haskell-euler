prob1 :: Int -> Int -> Int -> Int
prob1 x y limit = sum [a | a <- [1..limit], a `mod` x == 0 || a `mod` y == 0]
