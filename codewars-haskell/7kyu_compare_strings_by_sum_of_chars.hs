import Data.Char

compare' :: Maybe String -> Maybe String -> Bool
compare' x Nothing = compare' x (Just "")
compare' Nothing x = compare' (Just "") x
compare' (Just str1) (Just str2) = deal str1 == deal str2
  where deal str = sum $ map (ord.toUpper) $ if any (not.isLetter) str then "" else str
