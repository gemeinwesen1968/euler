even_fib :: Int -> Int
even_fib 1 = 2
even_fib 2 = 8
even_fib x = 4 * (even_fib (x - 1)) + (even_fib (x - 2))

sum_even_fibs :: Int -> Int -> Int
sum_even_fibs n acc
 | val > 4000000 = acc
 | otherwise = sum_even_fibs (n + 1) (acc + val)
 where val = even_fib n


main :: IO ()
main = print $ (sum_even_fibs 1 0)