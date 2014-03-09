Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a =/= b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

> import Data.List (nub)
>  
> properDivisors :: Int -> [Int]
> properDivisors n = nub [m | m <- [1..(n - 1)], n `mod` m == 0]
>  
> d :: Int -> Int
> d = sum . properDivisors
> 
> answer :: Int
> answer = 
>   let pairs = [(n, d n) | n <- [1..10000]]
>       amiciable (n, d) = (or . map (\(m,b) -> m /= n && m == d && b == n) ) pairs
>   in (sum . map fst . filter amiciable) pairs
>  
> main :: IO ()
> main = print answer
