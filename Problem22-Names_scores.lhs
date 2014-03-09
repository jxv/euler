Using [names.txt](http://projecteuler.net/project/names.txt) (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938  53 = 49714.

What is the total of all the name scores in the file?

> import Data.List (sort)  
>  
> scores :: [String] -> [Integer]
> scores = zipWith (\order name -> order * scoreName name) [1..] . sort
>   where 
>     scoreName = sum . map scoreLetter
>     scoreLetter = fromIntegral . subtract (fromEnum 'A' - 1) . fromEnum
>  
> answer :: IO Integer
> answer = 
>   do content <- readFile "names.txt"
>      let content' = read ("[" ++ content ++ "]")
>      (return . sum . scores) content'
>  
> main :: IO () 
> main =
>   do ans <- answer
>      print ans

