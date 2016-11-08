import Data.Char

traval :: String -> [Int] -> [Int]
traval [] ys = ys
traval (x:xs) ys = traval xs ys'
  where ind = (ord x) - (ord 'a')
        ys' = take ind ys ++ (ys!!(ind+1)+1 : drop (ind+1) ys)

myShow :: [Int] -> [Int] -> [String]
myShow [] [] = [[]]
myShow (x:xs) (y:ys)
  | x == y = (if x==0 then "" else '/':'=':':':[chx|_<-[1..x]]) : myShow xs ys
  | x >  y = ('/':'1':':':[chx|_<-[1..x]]) : myShow xs ys
  | x <  y = ('/':'2':':':[chy|_<-[1..y]]) : myShow xs ys
  where chx = chr (ord 'a' + x)
        chy = chr (ord 'a' + y)

mix :: String -> String -> String
mix s1 s2 = tail $ concat $ myShow xs1 xs2
  where xs1 = traval (map toLower s1) xs
        xs2 = traval (map toLower s2) xs
        xs  = take 26 $ repeat 0


