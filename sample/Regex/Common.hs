module Common where

data NFA = Finite | Compare Char NFA | Split NFA NFA

instance Show NFA where
  show = const "#<NFA>"

type Node = NFA -> NFA

type BinOp a = a -> a -> a

split :: BinOp Node
split f g x = Split (f x) (g x)

safeTail :: [a] -> (Maybe [a])
safeTail [] = Nothing
safeTail xs = Just (tail xs)
