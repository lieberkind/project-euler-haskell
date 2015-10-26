multipleOf3or5 :: Int -> Bool
multipleOf3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0

problem1 :: Int
problem1 = sum [x | x <- [1..999], multipleOf3or5 x]
