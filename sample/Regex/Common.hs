module Common where

data Op = Finite | Compare Char Op | If Op Op Op

type NFA = Op -> Op

split :: NFA -> NFA -> NFA
split f g x = If (f x) Finite (g x)

atomic :: NFA -> NFA -> NFA
atomic f g x = If (f Finite) x (g x)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)
