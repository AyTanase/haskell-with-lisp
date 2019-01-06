module Common where
data NFA = Finite | Compare Char NFA | Split NFA NFA
instance Show NFA where
  show = (const "#<NFA>")
type End a = (a -> a)
type BinOp a = (a -> (End a))
destribute :: ((b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d)
destribute f g h x = (f (g x) (h x))
