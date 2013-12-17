A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.

> answer :: Integer
> answer = foldr1 max [ c
>                     | a <- [100..999], b <- [100..999]
>                     , let c = a * b, let d = show c
>                     , reverse d == d 
>                     ]
> 
> main :: IO ()
> main = print answer
