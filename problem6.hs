problem6 = do
    let squareOfSums = (sum [1..100]) ^ 2
    let sumOfSquares = sum [x*x | x <- [1..100]]
    squareOfSums - sumOfSquares
