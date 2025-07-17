isPalindrome :: Int -> Bool
isPalindrome n = show n == reverse (show n)

maxPalindrom :: Int
maxPalindrom = 
    maximum . filter isPalindrome $
    [x * y | x <- [999, 998..100], y <- [999, 998..100]]

main :: IO ()
main = print maxPalindrom -- 87 ms