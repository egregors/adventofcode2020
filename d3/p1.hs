main :: IO ()
main = interact getTreesCount

fn :: Num b => (Int, b) -> [Char] -> (Int, b)
fn acc el = (pos + 3, newCount)
  where
    (pos, count) = acc
    newCount = if concat (repeat el) !! pos == '#' then count + 1 else count

getTreesCount input = show . snd . foldl fn (0, 0) $ lines input