import Data.List

partlist :: [String] -> [(String, String)]
partlist strList = map deal [1..length strList -1]
  where deal n = let (f, s) = splitAt n strList in (intercalate " " f, intercalate " " s)
