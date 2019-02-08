module Exec where
import Common
import Control.Applicative

exec :: Num a => Op -> String -> Maybe a
exec = exec' 0
  where
    exec' n Finite _ = Just n
    exec' n (Compare x rx) (_v8:xs)
      | (x == _v8) = exec' (n + 1) rx xs
    exec' _ (Compare _ _) _ = Nothing
    exec' n (Split p q) xs = (exec' n p xs) <|> (exec' n q xs)

match :: Num a => Op -> String -> Maybe (a, a)
match = match' 0
  where
    match' n rx xs = (fmap ((,) n) (exec rx xs)) <|> ((match' (n + 1) rx) =<< (safeTail xs))
