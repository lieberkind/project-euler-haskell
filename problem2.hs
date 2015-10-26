fib :: Int -> Int
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

fibsUpTo :: Int -> [Int]
fibsUpTo max = do
    let helper x fibList = do
        let currentFib = fib x
        if currentFib > max then fibList else helper (x + 1) (fibList ++ [currentFib])
    helper 1 []

problem2 :: Int
problem2 = sum [x | x <- fibsUpTo 4000000, even x]

-- Much nicer solution using takeWhile and function composition
problem2' :: Int
problem2' = sum . filter even . takeWhile (<4000000) $ map fib [1..]
