sums_to_k :: Int -> Int
sums_to_k k = k * (k + 1) `div` 2

sums_mult :: Int -> Int -> Int
sums_mult n k = (sums_to_k ((k - 1) `div` n)) * n

result :: Int -> Int -> Int -> Int
result a b k = (sums_mult a k) + (sums_mult b k) - (sums_mult (a*b) k)

main :: IO ()
main = print $ result 3 5 1000