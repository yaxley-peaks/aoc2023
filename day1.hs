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
process s = let maybes = map doThing $ tails s
                digits = catMaybes maybes
                first = head digits
                lst = last digits
             in 10 * first + lst

doThing :: String -> Maybe Int
doThing ('z':'e':'r':'o':_) = return 0
doThing ('o':'n':'e':_) = return 1
doThing ('t':'w':'o':_) = return 2
doThing ('t':'h':'r':'e':'e':_) = return 3
doThing ('f':'o':'u':'r':_) = return 4
doThing ('f':'i':'v':'e':_) = return 5
doThing ('s':'i':'x':_) = return 6
doThing ('s':'e':'v':'e':'n':_) = return 7
doThing ('e':'i':'g':'h':'t':_) = return 8
doThing ('n':'i':'n':'e':_) = return 9
doThing (x:_) | isDigit x = return (read [x])
                 | otherwise = Nothing
doThing [] = Nothing
