A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a2 + b2 = c2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

> triplets :: [(Integer, Integer, Integer)]
> triplets = [(a,b,c) | c <- [1..],
>                       b <- [1..(c - 1)],
>                       a <- [1..(b - 1)],
>                       a ^ 2 + b ^ 2 == c ^ 2]
> 
> triplet1000 :: (Integer, Integer, Integer)
> triplet1000 = head (filter (\(a,b,c) -> a + b + c == 1000) triplets)
>  
> answer :: Integer
> answer = (\(a,b,c) -> a * b * c) triplet1000
> 
> main :: IO ()
> main = print answer
