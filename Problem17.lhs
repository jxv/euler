If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

> name :: Int -> String
> name num = case (show num) of (a:[]) -> defaultDigit a
>                               (a:b:[]) -> handleTens a b
>                               (a:b:c:[]) -> handleHundreds a b c
>                               (a:b:c:d:[]) -> handleThousands a b c d
>                               _ -> [] 
> 
> handleTens :: Char -> Char -> String
> handleTens a b = if b == '0'
>                     then tensDigit a 
>                     else case a of '1' -> teenDigit b
>                                    _ -> (tensDigit a) ++ (defaultDigit b)
> 
> handleHundreds :: Char -> Char -> Char -> String
> handleHundreds a b c = (defaultDigit a) ++ hundred ++ (if b == '0' && c == '0'
>                                                           then ""
>                                                           else and' ++ (handleTens b c))
>  
> handleThousands :: Char -> Char -> Char -> Char -> String
> handleThousands '1' '0' '0' '0' = (defaultDigit '1') ++ thousand
> handleThousands _ _ _ _         = []
> 
> defaultDigit :: Char -> String
> defaultDigit dgt = case dgt of '0' -> "zero"
>                                '1' -> "one"
>                                '2' -> "two"
>                                '3' -> "three"
>                                '4' -> "four"
>                                '5' -> "five"
>                                '6' -> "six"
>                                '7' -> "seven"
>                                '8' -> "eight"
>                                '9' -> "nine"
>                                _   -> ""
> 
> 
> tensDigit :: Char -> String
> tensDigit dgt = case dgt of '1' -> "ten"
>                             '2' -> "twenty"
>                             '3' -> "thirty"
>                             '4' -> "forty"
>                             '5' -> "fifty"
>                             '6' -> "sixty"
>                             '7' -> "seventy"
>                             '8' -> "eighty"
>                             '9' -> "ninety"
>                             _   -> ""
> 
> 
> teenDigit :: Char -> String
> teenDigit dgt = case dgt of '1' -> "eleven"
>                             '2' -> "twelve"
>                             '3' -> "thirteen"
>                             '4' -> "fourteen"
>                             '5' -> "fifteen"
>                             '6' -> "sixteen"
>                             '7' -> "seventeen"
>                             '8' -> "eighteen"
>                             '9' -> "nineteen"
>                             _   -> ""
> 
> and' :: String
> and' = "and"
> 
> dash :: String
> dash = "-"
> 
> hundred :: String 
> hundred = "hundred"
> 
> thousand :: String 
> thousand = "thousand"
> 
> answer :: Int
> answer = (sum . map (length . name)) [1..1000]
> 
> main :: IO () 
> main = print answer
