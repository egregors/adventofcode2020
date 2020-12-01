main :: IO ()
main = interact f

f :: String -> String
f input =
  show . product . head $
    [ [a, b]
      | a <- l,
        b <- l,
        a /= b,
        a + b == 2020
    ]
  where
    l = map (\x -> read x :: Int) $ words input
