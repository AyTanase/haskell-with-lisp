module Common where

data Op = Finite | Cut | Compare Char Op | If Op Op Op

type NFA = Op -> Op

split :: NFA -> NFA -> NFA
split f g x = If (f x) Finite (g x)

atomic :: NFA -> NFA -> NFA
atomic f g x = If (f Finite) x (g x)

cut :: NFA -> NFA
cut f x = If (f Finite) x Cut

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs
