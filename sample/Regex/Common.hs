module Common where
import GHC.Base

data Op = Finite | Compare Char Op | Split Op Op | Atomic Op Op Op

type NFA = Op -> Op

split :: NFA -> NFA -> NFA
split = liftA2 Split

atomic :: NFA -> NFA -> NFA
atomic f g x = Atomic (f Finite) x (g x)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)
