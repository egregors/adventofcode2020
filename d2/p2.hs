main :: IO ()
main = interact getValidPassCount

getValidPassCount :: String -> String
getValidPassCount raw = show $ foldr ((\e sum -> if e then sum + 1 else sum) . isValid) 0 $ lines raw

isValid :: [Char] -> Bool
isValid p
  | shouldBeCh == ch && shouldn'tBeCh /= ch || shouldBeCh /= ch && shouldn'tBeCh == ch = True
  | otherwise = False
  where
    ws = words p
    pass = last ws
    ch = head . filter (/= ':') $ ws !! 1
    rules = split '-' $ head ws
    shouldBeId = read $ head rules :: Int
    shouldn'tBeId = read $ last rules :: Int
    shouldBeCh = getCh shouldBeId
    shouldn'tBeCh = getCh shouldn'tBeId
    getCh idx = pass !! (idx - 1)

split :: Char -> String -> [String]
split c xs = case break (== c) xs of
  (ls, "") -> [ls]
  (ls, _ : rs) -> ls : split c rs
