2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

> divisable :: Integral a => a -> a -> Bool
> divisable x y = x `mod` y == 0
>
> smallestDivisable :: Integral a => [a] -> a
> smallestDivisable range = step 1
>   where step n = if and (map (divisable n) range)
>                    then n
>                    else step (n + 1)
>
> answer :: Integral a => a 
> answer = smallestDivisable [1..20]
>
> main :: IO ()
> main = print answer
