If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.

> import Control.Applicative
>
> multipleOf :: (Integral a) => a -> a -> Bool
> multipleOf a b = b `mod` a == 0
>
> multiples :: [Integer]
> multiples = filter (\n -> or $ ($ n) <$> multipleOf <$> [3,5]) [1..]
>
> answer :: Integer
> answer = sum (takeWhile (< 1000) multiples)
>
> main :: IO ()
> main = print answer
