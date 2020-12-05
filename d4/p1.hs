import qualified Data.Map as Map
import Data.Maybe (isNothing)

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

isValid :: Passport -> Bool
isValid p
  | isNothing (passByr p) = False
  | isNothing (passIyr p) = False
  | isNothing (passEyr p) = False
  | isNothing (passHgt p) = False
  | isNothing (passHcl p) = False
  | isNothing (passEcl p) = False
  | isNothing (passPid p) = False
  | otherwise = True

getValidPassportsCount :: String -> String
getValidPassportsCount input = show . length $ filter isValid $ getPassportList input

input' =
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
  \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
  \\n\
  \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
  \hcl:#cfa07d byr:1929\n\
  \\n\
  \hcl:#ae17e1 iyr:2013\n\
  \eyr:2024\n\
  \ecl:brn pid:760753108 byr:1931\n\
  \hgt:179cm\n\
  \\n\
  \hcl:#cfa07d eyr:2025 pid:166559648\n\
  \iyr:2011 ecl:brn hgt:59in\n"