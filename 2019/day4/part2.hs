
range :: [Int]
range = [193651..649729]

isSorted :: Ord a => [a] -> Bool
isSorted []             = True
isSorted [_]            = True
isSorted (x1:xs@(x2:_)) = x1 <= x2 && isSorted xs

containsDouble :: Eq a => [a] -> Bool
containsDouble []       = False
containsDouble [_]      = False
containsDouble [x1, x2] = x1 == x2
containsDouble (x1:x2:x3:xs)
  | x1 == x2 && x2 /= x3 = True
  | x1 == x2 && x2 == x3 = containsDouble . dropWhile (== x3) $ xs
  | x1 /= x2             = containsDouble (x2:x3:xs)

isPassword :: Int -> Bool
isPassword n =
  let s = show n in
  isSorted s && containsDouble s

countPasswords :: Int
countPasswords = length . filter isPassword $ range

