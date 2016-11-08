import Data.Char
import Data.List

traval :: String -> [Int] -> [Int]
traval [] ys = ys
traval (x:xs) ys = traval xs ys'
  where ind = (ord x) - (ord 'a')
        ys' = take ind ys ++ (ys!!(ind+1)+1 : drop (ind+1) ys)

getCount :: String -> [Int]
getCount xs = map (\s -> length s - 1) $ group.sort $ ['a'..'z'] ++ (map toLower $ filter isLetter xs)

myShow :: [Int] -> [Int] -> Int -> [String]
myShow [] [] shift = [[]]
myShow (x:xs) (y:ys) shift
  | x == y = (if x==0 then "" else '/':'=':':':[chx|_<-[1..x]]) : myShow xs ys (shift+1)
  | x >  y = ('/':'1':':':[chx|_<-[1..x]]) : myShow xs ys (shift+1)
  | x <  y = ('/':'2':':':[chy|_<-[1..y]]) : myShow xs ys (shift+1)
  where chx = chr (ord 'a' + shift)
        chy = chr (ord 'a' + shift)

mix :: String -> String -> String
mix s1 s2 = tail $ concat $ myShow xs1 xs2 0
  where xs1 = getCount s1
        xs2 = getCount s2


