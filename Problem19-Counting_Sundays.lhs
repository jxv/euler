You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

> type Year = Int
> 
> data Month =
>     Jan
>   | Feb
>   | Mar
>   | Apr
>   | May
>   | Jun
>   | Jul
>   | Aug
>   | Sep
>   | Oct
>   | Nov
>   | Dec
>   deriving (Show, Eq, Enum, Bounded)
> 
> data WeekDay =
>     Sun
>   | Mon
>   | Tue
>   | Wed
>   | Thu
>   | Fri
>   | Sat
>   deriving (Show, Eq, Enum, Bounded)
> 
> data Date = Date
>   { dateYear :: Year
>   , dateMonth :: Month
>   , dateDay :: Int
>   } deriving (Show)
 
> bndRng :: (Enum b, Bounded b) => [b]
> bndRng = [minBound..maxBound]
 
> week :: [WeekDay] 
> week = bndRng
>
  
> dayCountPerMonth :: Year -> Month -> Int
> dayCountPerMonth year month = 
>  let leapyear = (year `rem` 4 == 0) && (year `rem` 100 /= 0 || year `rem` 400 == 0)
>  in case month of
>       Feb -> if leapyear then 29 else 28
>       Apr -> 30
>       Jun -> 30
>       Sep -> 30
>       Nov -> 30
>       _   -> 31

> allWeekDaysInRange :: Year -> Year -> WeekDay -> [(WeekDay, Date)]
> allWeekDaysInRange syear eyear sday =
>   let days = [sday..maxBound] ++ (concat . repeat) week
>       dates = [Date y m d | y <- [syear..eyear], m <- bndRng, d <- [1..(dayCountPerMonth y m)]]
>   in zip days dates

> answer :: Int 
> answer = length [d | d <- allWeekDaysInRange 1900 2000 Mon
>                    , let date = snd d
>                    , dateYear date >= 1901 && dateYear date <= 2000
>                    , dateDay date == 1
>                    , fst d == Sun]

> main :: IO ()
> main = print answer
