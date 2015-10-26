isPalindrome :: Int -> Bool
isPalindrome x = x == (read . reverse . show $ x)

problem4 :: Int
problem4 = do
    let range = [999,998..100]
    maximum [x*y | x <- range, y <- range, isPalindrome (x*y)]
