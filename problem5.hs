isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy x y = x `mod` y == 0

isDivisibleByRange :: Int -> Int -> Int -> Bool
isDivisibleByRange x min max = foldl (&&) True $ map (isDivisibleBy x) [min..max]

problem5 :: Int
problem5 = head [x | x <- [380,760..], isDivisibleByRange x 1 20]

