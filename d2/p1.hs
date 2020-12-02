import Debug.Trace

main :: IO ()
main = interact getValidPassCount

getValidPassCount :: String -> String
getValidPassCount raw = show $ foldr ((\e sum -> if e then sum + 1 else sum) . isValid) 0 $ lines raw

isValid :: [Char] -> Bool
isValid p
  | chCount <= maxCount && chCount >= minCount = True
  | otherwise = False
  where
    ws = words p
    pass = last ws
    ch = head . filter (/= ':') $ ws !! 1
    rules = split '-' $ head ws
    minCount = read $ head rules :: Int
    maxCount = read $ last rules :: Int
    chCount = foldr (\el acc -> if el == ch then acc + 1 else acc) 0 pass

split :: Char -> String -> [String]
split c xs = case break (== c) xs of
  (ls, "") -> [ls]
  (ls, _ : rs) -> ls : split c rs