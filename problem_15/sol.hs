factorial :: Integer -> Maybe Integer
factorial n
  | n < 0     = Nothing
  | otherwise = Just (product [1..n])

comb :: Integer -> Maybe Integer
comb n
  | n < 0     = Nothing
  | otherwise = do
      f1 <- factorial (2 * n)
      f2 <- factorial n
      return $ f1 `div` (f2 * f2)

main :: IO ()
main = case comb 20 of
  Just result -> print result
  Nothing     -> putStrLn "Invalid input: n must be non-negative."