import qualified Data.Set as Set

main :: IO ()
main = interact getAnswersCount

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

getAnswersCount :: String -> String
getAnswersCount input = show . sum . map getGroupCount $ splitList "" $ lines input

getGroupCount :: [String] -> Int
getGroupCount xs = foldl (\s el -> if all (elem el) xs then s + 1 else s) 0 allQuestions
  where
    allQuestions = Set.toList $ Set.fromList $ concat xs