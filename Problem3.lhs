The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

> nums :: (Integral a, Num b) => a -> b
> nums = fromIntegral
>
> divisable :: Integer -> Integer -> Bool
> divisable x y = x `mod` y == 0
>
> primes :: [Integer]
> primes = 2 : prime 2 [2]
>
> prime x n = let p = nextPrime x n
>                 ps = p : n
>             in p : prime p ps
>
> nextPrime :: Integer -> [Integer] -> Integer
> nextPrime n ps = if or (map (divisable n) ps)
>                       then nextPrime (n + 1) ps
>                       else n
> 
> answer :: Integer
> answer = let number = 600851475143
>          in last ( filter (divisable number) (takeWhile (< number) primes) )
> 
> main :: IO ()
> main = print answer
