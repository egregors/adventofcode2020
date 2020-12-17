import qualified Data.Map as Map

main :: IO()
main = interact getAcc

input' =
  "nop +0\n\
  \acc +1\n\
  \jmp +4\n"
  -- \acc +3\n\
  -- \jmp -3\n\
  -- \acc -99\n\
  -- \acc +1\n\
  -- \jmp -4\n\
  -- \acc +6\n"

data Cmd a = Nop a | Acc a | Jmp a deriving (Show)

data Op = Op
  { cmd :: Cmd Int,
    isDone :: Bool
  }
  deriving (Show)

fromString :: String -> Op
fromString s
  | cmd == "nop" = Op (Nop val) False
  | cmd == "acc" = Op (Acc val) False
  | cmd == "jmp" = Op (Jmp val) False
  | otherwise = error "wrong cmd type"
  where
    cmd = take 3 s
    sign = s !! 4
    unsignVal = read $ drop 5 s :: Int
    val = if sign == '-' then unsignVal * (-1) else unsignVal

makeMapFromString :: (Ord k, Num k, Enum k) => String -> Map.Map k Op
makeMapFromString s = Map.fromList $ zip [0 ..] $ map fromString $ lines s

execOp :: Op -> (Int -> Int, Int -> Int)
execOp (Op (Nop _) _) = ((+ 1), id)
execOp (Op (Acc n) _) = ((+ 1), (+ n))
execOp (Op (Jmp n) _) = ((+ n), id)


eval :: Int -> Int -> Map.Map Int Op -> Maybe Int
eval position acc xs
  | isDone cmd = Nothing
  | position == length xs - 1 = Just (newAccF acc)
  | otherwise = eval (newPositionF position) (newAccF acc) newXs
  where
    (Just cmd) = Map.lookup position xs
    (newPositionF, newAccF) = execOp cmd
    newXs = Map.update (\(Op op _) -> Just (Op op True)) position xs

getAcc :: String -> String
getAcc s = show . eval 0 0 $ makeMapFromString s



