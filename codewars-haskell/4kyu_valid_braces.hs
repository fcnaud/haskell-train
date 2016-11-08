import Data.Maybe

data Stack a = Stack [a] deriving (Show)

empty :: Stack a -> Bool
empty (Stack []) = True
empty (Stack _ ) = False

push :: a -> Stack a -> Stack a
push x (Stack st) = Stack (x:st)

pop :: Stack a -> Maybe (Stack a)
pop (Stack st) = case empty (Stack st) of
  False -> Just (Stack (tail st))
  True  -> Nothing

top :: Stack a -> Maybe a
top (Stack st) = case empty (Stack st) of
  False -> Just (head st)
  True  -> Nothing

left :: Char -> Bool
left '{' = True
left '[' = True
left '(' = True
left  _  = False

isPair :: Char -> Char -> Bool
isPair '(' ')' = True
isPair '[' ']' = True
isPair '{' '}' = True
isPair  _   _  = False

check :: String -> Stack Char -> Bool
check [] st = empty st
check (x:xs) st
  | left x    = check xs st'
  | otherwise = 
      if empty st then False
      else
        if isPair (fromJust $ top st) x then check xs (fromJust st'') 
        else False 
  where st'   = push x st
        st''  = pop st
validBraces :: String -> Bool
validBraces str = check str (Stack [])

