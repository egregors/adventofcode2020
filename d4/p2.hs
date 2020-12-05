import Data.Char (isDigit)
import Data.List (isInfixOf)
import qualified Data.Map as Map

main :: IO ()
main = interact getValidPassportsCount

tail' :: [a] -> [a]
tail' (_ : xs) = xs
tail' [] = []

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

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d s = x : splitOn d (tail' s')
  where
    (x, s') = span (/= d) s

data Passport = Passport
  { passByr :: Maybe String,
    passIyr :: Maybe String,
    passEyr :: Maybe String,
    passHgt :: Maybe String,
    passHcl :: Maybe String,
    passEcl :: Maybe String,
    passPid :: Maybe String,
    passCid :: Maybe String
  }
  deriving (Show)

passport =
  Passport
    { passByr = Nothing,
      passIyr = Nothing,
      passEyr = Nothing,
      passHgt = Nothing,
      passHcl = Nothing,
      passEcl = Nothing,
      passPid = Nothing,
      passCid = Nothing
    }

toPair :: [a] -> (a, a)
toPair [a, b] = (a, b)

toKV :: [[Char]] -> [([Char], [Char])]
toKV = map (toPair . splitOn ':')

passportFromList :: Map.Map [Char] [Char] -> Passport
passportFromList xs = Passport byr iyr eyr hgt hcl ecl pid cid
  where
    byr = Map.lookup "byr" xs
    iyr = Map.lookup "iyr" xs
    eyr = Map.lookup "eyr" xs
    hgt = Map.lookup "hgt" xs
    hcl = Map.lookup "hcl" xs
    ecl = Map.lookup "ecl" xs
    pid = Map.lookup "pid" xs
    cid = Map.lookup "cid" xs

getPassportList :: String -> [Passport]
getPassportList input = map (passportFromList . Map.fromList . toKV . words . unwords) $ splitList "" $ lines input

getValidPassportsCount :: String -> String
getValidPassportsCount input = show . length $ filter isValidPassport $ getPassportList input

isValidPassport :: Passport -> Bool
isValidPassport (Passport byr iyr eyr hgt hcl ecl pid _) =
  and
    [ isValidByr byr,
      isValidIyr iyr,
      isValidEyr eyr,
      isValidHgt hgt,
      isValidHcl hcl,
      isValidEcl ecl,
      isValidPid pid
    ]

-- fields validators

isValidYear :: String -> Int -> Int -> Bool
isValidYear x from to
  | not $ all isDigit x = False
  | y < from || y > to = False
  | otherwise = True
  where
    y = read x :: Int

isValidByr :: Maybe String -> Bool
isValidByr Nothing = False
isValidByr (Just x) = isValidYear x 1920 2002

isValidIyr :: Maybe String -> Bool
isValidIyr Nothing = False
isValidIyr (Just x) = isValidYear x 2010 2020

isValidEyr :: Maybe String -> Bool
isValidEyr Nothing = False
isValidEyr (Just x) = isValidYear x 2020 2030

isValidHgt :: Maybe String -> Bool
isValidHgt Nothing = False
isValidHgt (Just x)
  | not ("in" `isInfixOf` x || "cm" `isInfixOf` x) = False
  | "cm" `isInfixOf` x && (cmHgt < 150 || cmHgt > 193) = False
  | "in" `isInfixOf` x && (inHgt < 59 || inHgt > 76) = False
  | otherwise = True
  where
    cmHgt = read $ takeWhile (/= 'c') x :: Int
    inHgt = read $ takeWhile (/= 'i') x :: Int

isValidHcl :: Maybe String -> Bool
isValidHcl Nothing = False
isValidHcl (Just x)
  | '#' `notElem` x = False
  | not $ all (`elem` validChars) x = False
  | otherwise = True
  where
    validChars = ['#'] ++ ['0' .. '9'] ++ ['a' .. 'f']

isValidEcl :: Maybe String -> Bool
isValidEcl Nothing = False
isValidEcl (Just x) = x `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPid :: Maybe String -> Bool
isValidPid Nothing = False
isValidPid (Just x)
  | not $ all isDigit x = False
  | length x /= 9 = False
  | otherwise = True