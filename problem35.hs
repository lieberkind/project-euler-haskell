-- | sqrt max < nextPrime = primes
-- | otherwise = findPrimes (filter (\x -> x `mod` nextPrime) possibles) (nextPrime :: primes)
--    where nextPrime = head possibles
hasEvenDigit :: Int -> Bool
hasEvenDigit x
    | x `mod` 2 == 0        = True
    | x `div` 10 == 0       = x `mod` 2 == 0
    | otherwise             = hasEvenDigit $ x `div` 10

candidatesUpTo :: Int -> [Int]
candidatesUpTo 1 = []
candidatesUpTo max = do
    let findPrimes possibles primes
            | nextPrime ^ 2 > max   = (reverse primes) ++ possibles
            | otherwise             = findPrimes newPossibles (nextPrime : primes)
            where   nextPrime = head possibles
                    newPossibles = filter (\x -> x `mod` nextPrime /= 0) possibles
    findPrimes [x | x <- [2..max], not (hasEvenDigit x)] []

-- Get the number of digits in a number
digitCount :: Int -> Int
digitCount x = (+1) . floor $ logBase 10 a
    where a = fromIntegral x

rotate :: Int -> Int
rotate x = floor (( fromIntegral x) / 10) + (10 ^ (digits - 1)) * lastDigit
    where   a = fromIntegral x
            lastDigit = x `mod` 10
            digits = digitCount x

rotations :: Int -> [Int]
rotations x = do
    let getRotations start list
            | rotation == x  = rotation : list
            | otherwise      = getRotations rotation (rotation : list)
            where rotation = rotate start
    getRotations x []


candidates = candidatesUpTo 1000000
isCandidate x = elem x candidates

isCircularPrime :: Int -> Bool
isCircularPrime x = do
    let possiblePrimes = rotations x
    foldl (&&) True $ map (\x -> isCandidate x) possiblePrimes

--problem35 :: Int
problem35 = filter isCircularPrime $ candidates
