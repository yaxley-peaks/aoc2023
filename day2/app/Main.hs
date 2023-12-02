module Main where

import Data.List.Split

parseInput :: String -> [(Int, [[String]])]
parseInput inp =
  map
    ( \x -> do
        let [id, sets] = splitOneOf ":" x
        let cubes = chunksOf 2 $ filter (not . null) $ splitOneOf ";, " sets
        (read $ drop 5 id, cubes) -- length "Game " = 5
    )
    $ lines inp

isValid :: [String] -> Bool
isValid [num, "red"] = read num <= 12
isValid [num, "green"] = read num <= 13
isValid [num, "blue"] = read num <= 14
isValid _ = undefined

part1 :: String -> Int
part1 inp =
  let parsed = parseInput inp
      invalidGames = filter (\(num, draws) -> not (all isValid draws)) parsed
      sumIV = sum (fst <$> invalidGames)
  in sum [1 .. length parsed] - sumIV

part2 :: String -> Int
part2 inp =
  let parsed = parseInput inp
  in  sum $ product . findMaxByColor <$> parsed

findMaxByColor :: (Int, [[String]]) -> [Int]
findMaxByColor (_, draws) =
  map
    ( \c ->
        maximum $
          map (\[num, _] -> read num) $
            filter
              (\x -> x !! 1 == c)
              draws
    )
    ["red", "green", "blue"]

main :: IO ()
main = do
  i <- readFile "input.txt"
  print $ part1 i
  print $ part2 i
