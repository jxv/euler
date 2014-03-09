Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.

[img](http://projecteuler.net/project/images/p_015.gif)

How many such routes are there through a 20x20 grid?

> latticePath :: Integer -> Integer
> latticePath n = let path x y | x == 1 || y == 1 = 1
>                              | otherwise = path (x - 1) y + path x (y - 1)
>                 in path n n
> 
> latticePath' :: Int -> Integer
> latticePath' n = (last . (foldr1 (.) (id : replicate (n - 1) sieve))) [2]
>                where sieve r = let rs = 1 : zipWith (+) r rs
>                                in (tail rs) ++ [(last rs) * 2]
>  
> answer :: Integer
> answer = latticePath' 20
> 
> main :: IO ()
> main = print answer
