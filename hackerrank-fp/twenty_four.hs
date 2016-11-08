data Exp = Val Double 
         | Plus Exp Exp
         | Sub Exp Exp
         | Mult Exp Exp
         | Div Exp Exp
         deriving (Show, Eq)

eval :: Exp -> Double
eval (Val a) = a
eval (Plus a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a / eval b

showExp :: Exp -> String
showExp (Val a) = show a
showExp (Plus a b) = "(" ++ showExp a ++ "+" ++ showExp b ++ ")"
showExp (Sub a b) = "(" ++ showExp a ++ "-" ++ showExp b ++ ")"
showExp (Mult a b) = "(" ++ showExp a ++ "*" ++ showExp b ++ ")"
showExp (Div a b) = "(" ++ showExp a ++ "/" ++ showExp b ++ ")"

main = do
  let str = (Plus (Val 3) (Val 3)) `Div` (Val 2)
  
  putStr $ showExp str
  putStr " = "
  putStrLn.show $ eval str
  
