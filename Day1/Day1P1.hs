import Data.Char (isSpace)
import Data.List (sort, unzip)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

processInput :: String -> ([Int], [Int])
processInput input =
  let pairs = [(read x, read y) | line <- lines input, let items = words (trim line), length items == 2, let [x, y] = items]
   in unzip pairs

solve :: [Int] -> [Int] -> Int
solve x y =
  let xs = sort x
      ys = sort y
   in sum (zipWith (\x y -> abs (x - y)) xs ys)

main :: IO ()
main = do
  input <- getContents
  let (d1, d2) = processInput input
  print (solve d1 d2)
