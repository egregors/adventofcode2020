main :: IO ()
main = interact getTreesCount

baseFn :: Num b => Int -> (Int, b) -> [Char] -> (Int, b)
baseFn step acc el = (newPos, newCount)
  where
    (pos, count) = acc
    newPos = pos + step
    point = concat (repeat el) !! pos
    newCount = if point == '#' then count + 1 else count

getTreesCountDown1 :: String -> Integer
getTreesCountDown1 input =
  product $
    map (\fn -> snd . foldl fn (0, 0) $ lines input) $
      [baseFn step | step <- [1, 3 .. 7]]

getTreesCountDown2 :: String -> Integer
getTreesCountDown2 input = snd . foldl (baseFn 1) (0, 0) $ input'
  where
    input' = map snd $ filter (\(idx, _) -> odd idx) $ zip [1 ..] $ lines input

getTreesCount :: String -> String
getTreesCount input = show (getTreesCountDown1 input * getTreesCountDown2 input)