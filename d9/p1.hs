import qualified Data.Set as S

main :: IO ()
main = interact getNumber

getNumber :: String -> String
getNumber s = show $ checkSums (take 25 l) (drop 25 l)
  where
    l = getIntList s

getIntList :: String -> [Int]
getIntList s = map (\e -> read e :: Int) $ lines s

getSums :: (Ord a, Num a) => [a] -> [a]
getSums xs = S.toList . S.fromList $ [a + b | a <- xs, b <- xs]

checkSums preamble@(p : ps) ns@(x : xs)
  | x `notElem` sums = x
  | otherwise = checkSums (ps ++ [x]) xs
  where
    sums = getSums preamble