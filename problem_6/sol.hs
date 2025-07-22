sumOfSquares :: Int -> Int
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6

squareOfSums :: Int -> Int
squareOfSums n = (n * (n + 1) `div` 2) ^ 2

main :: IO ()
main = print $ ((sumOfSquares 100) - (squareOfSums 100))