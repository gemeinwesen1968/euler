isPrime :: Integer -> Bool
isPrime n = all (\d -> n `mod` d /= 0) [2 .. floor (sqrt (fromIntegral n))]

primes :: [Integer]
primes = [x | x <- [2..], isPrime x]

sumOfPrimes :: Integer -> Integer
sumOfPrimes p = sum (takeWhile (< p) primes)

main :: IO ()
main = print (sumOfPrimes 2000000)
