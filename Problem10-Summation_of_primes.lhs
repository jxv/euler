The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.

> import Data.Numbers.Primes -- package primes
> 
> answer :: Integer
> answer = sum (takeWhile (< number) primes)
>   where number = 2000000
> 
> main :: IO ()
> main = print answer
