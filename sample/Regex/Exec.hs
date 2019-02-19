module Exec where
import Common
import Control.Applicative ((<|>))

exec :: Num a => Op -> String -> Maybe a
exec s = fmap fst . exec' s 0
  where
    exec' Finite n xs = Just (n, xs)
    exec' (Compare x p) n (v1:xs)
      | x == v1 = exec' p (n + 1) xs
    exec' (If p q r) n xs = maybe (exec' r n xs) (uncurry $ exec' q) (exec' p n xs)
    exec' _ _ _ = Nothing

match :: Num a => Op -> String -> Maybe (a, a)
match = match' 0
  where
    match' n r xs = fmap ((,) n) (exec r xs) <|> (match' (n + 1) r =<< safeTail xs)
