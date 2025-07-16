isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod` d /= 0) [2 .. floor (sqrt (fromIntegral n))]

primes :: [Integer]
primes = [x | x <- [2..], isPrime x]

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = helper n primes
 where 
    helper n (p:ps)
     | p * p > n = n
     | n `mod` p == 0 = helper (n `div` p) (p:ps)
     | otherwise = helper n ps

main :: IO ()
main = print $ largestPrimeFactor 600851475143