
By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?

> divisable :: Integer -> Integer -> Bool
> divisable x y = x `mod` y == 0
>
> primes :: [Integer]
> primes = 2 : prime 2 [2]
>   where
>     prime x n = let p = nextPrime x n
>                     ps = p : n
>                 in p : prime p ps
>     nextPrime n ps = if or (map (divisable n) ps)
>                        then nextPrime (n + 1) ps
>                        else n
> 
> answer :: Integer 
> answer = primes !! 10000 -- which is 10001 starting from 0
> 
> main :: IO ()
> main = print answer
