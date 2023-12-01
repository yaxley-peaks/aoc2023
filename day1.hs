import Data.Char (isDigit)
import Data.Maybe
import Data.List
-- Day 1

part1 :: String -> Int
part1 ss =
  let ls = lines ss
      a = head . filter isDigit <$> ls
      b = head . filter isDigit <$> (reverse <$> ls)
      z = zip a b
      nums = map (\(x, y) -> read [x, y]) z
   in sum nums

part2 :: String -> Int
part2 fp =
    let x = map process (lines fp)
    in sum x

-- part2 :: String -> Int
-- part2 ss =

main :: IO ()
main = do
  x <- readFile "inputs/day1.txt"
  print $ part1 x
  print $ part2 x


process :: String -> Int
process s = let maybes = map matchDigit $ tails s
                digits = catMaybes maybes
                first = head digits
                lst = last digits
             in 10 * first + lst

matchDigit :: String -> Maybe Int
matchDigit ('z':'e':'r':'o':_) = return 0
matchDigit ('o':'n':'e':_) = return 1
matchDigit ('t':'w':'o':_) = return 2
matchDigit ('t':'h':'r':'e':'e':_) = return 3
matchDigit ('f':'o':'u':'r':_) = return 4
matchDigit ('f':'i':'v':'e':_) = return 5
matchDigit ('s':'i':'x':_) = return 6
matchDigit ('s':'e':'v':'e':'n':_) = return 7
matchDigit ('e':'i':'g':'h':'t':_) = return 8
matchDigit ('n':'i':'n':'e':_) = return 9
matchDigit (x:_) | isDigit x = return (read [x])
                 | otherwise = Nothing
matchDigit [] = Nothing
