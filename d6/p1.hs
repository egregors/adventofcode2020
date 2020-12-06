import Data.Set (fromList, size)

main :: IO ()
main = interact getAnswersCount

getAnswersCount :: String -> String
getAnswersCount input =
  show
    . sum
    . map (size . fromList . concat)
    $ splitList "" $ lines input

splitList :: Eq a => a -> [a] -> [[a]]
splitList _ [] = []
splitList sep list = h : splitList sep t
  where
    (h, t) = split (== sep) list

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left, right)
  where
    (left, right') = break f s
    right = if null right' then [] else tail right'