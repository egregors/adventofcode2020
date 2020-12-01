main :: IO ()
main = interact f

f :: String -> String
f input =
  show . product . head $
    [ [a, b, c]
      | a <- l,
        b <- l,
        c <- l,
        a /= b && b /= c,
        a + b + c == 2020
    ]
  where
    l = map (\x -> read x :: Int) $ words input
