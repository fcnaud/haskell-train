gcdEx :: Int -> Int -> (Int,Int,Int)
gcdEx a 0 = (1, 0, a)
gcdEx a b = (x',y',d)
  where (x,y,d) = gcdEx b (a`mod`b)
        x' = y
        y' = x - (a`div`b)*y

getOneAns :: Int -> Int -> Int -> (Int,Int)
getOneAns a b ans = (x`div`d*ans, y`div`d*ans)
  where (x,y,d) = gcdEx a b 

-- 下面是主要计算，上面只是利用扩展欧几里得求出一组解

howmuch :: Int -> Int -> [[String]]
howmuch n' m' = map f [l..r]
  where n = min n' m'
        m = max n' m'
        l = max 0 (max (ceiling $ get1 n) (ceiling $ get2 n))
        r = min (floor $ get1 m) (floor $ get2 m)

        get1 = \x -> ((realToFrac x -1)/9 + 3 ) / 7
        get2 = \x -> ((realToFrac x -2)/7 + 4 ) / 9

        f t = ["M: "++(show $ c*9+1), "B: "++(show b), "C: "++(show c)]
          where c = -3+7*t
                b = -4+9*t

