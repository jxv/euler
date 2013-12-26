The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?

> import Data.Numbers.Primes -- package primes
> 
> answer :: Integer
> answer = let number = 600851475143 
>          in last (primeFactors number)
> 
> main :: IO ()
> main = print answer
