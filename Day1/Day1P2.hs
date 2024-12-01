import Data.Char (isSpace)
import Data.List (group, sort, unzip)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

processInput :: String -> ([Int], [Int])
processInput input =
  let pairs = [(read x, read y) | line <- lines input, let items = words (trim line), length items == 2, let [x, y] = items]
   in unzip pairs

getTimes :: (Ord a) => [a] -> [(a, Int)]
getTimes xs = map (\g -> (head g, length g)) (group (sort xs))

solve :: [Int] -> [Int] -> Int
solve left right =
  let rightCounts = getTimes right
      lookupCount x = sum [count | (y, count) <- rightCounts, y == x]
   in sum [x * lookupCount x | x <- left]

main :: IO ()
main = do
  input <- getContents
  let (d1, d2) = processInput input
  print (solve d1 d2)
