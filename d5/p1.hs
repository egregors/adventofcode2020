main :: IO ()
main = interact getMaxID

getP :: Int -> Int -> String -> Int
getP a b [x] = case x of
  x | x `elem` ['F', 'L'] -> a
  x | x `elem` ['B', 'R'] -> b
getP a b (x : xs) = case x of
  x | x `elem` ['F', 'L'] -> getP a (b - (b - a + 1) `div` 2) xs
  x | x `elem` ['B', 'R'] -> getP (a + (b - a + 1) `div` 2) b xs


getColumn :: [Char] -> Int
getColumn xs = getP 0 7 $ drop 7 xs

getRow :: [Char] -> Int
getRow xs = getP 0 127 $ take 7 xs

getId :: [Char] -> Int
getId input = getRow input * 8 + getColumn input

getMaxID :: String -> String
getMaxID input = show . maximum . map getId $ lines input