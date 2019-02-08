module Common where
import GHC.Base

data Op = Finite | Compare Char Op | Split Op Op

type NFA = Op -> Op

type BinOp a = a -> a -> a

split :: BinOp NFA
split = liftA2 Split

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)
