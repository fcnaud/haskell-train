
getLevel :: [Int] -> [Int]
getLevel xs = 1 : [xs!!ind+xs!!(ind+1)|ind<-[0..(length xs)-2]] ++ [1]

yanghui :: Int -> [[Int]]
yanghui 1 = [[1]]
yanghui 2 = [[1,1],[1]]
yanghui n = getLevel (preAns!!0) : preAns
  where preAns = yanghui (n-1)

pascalsTriangle :: Int -> [Int]
pascalsTriangle n = reverse.concat $ yanghui n
