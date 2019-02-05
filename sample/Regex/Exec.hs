module Exec where
import Common
import Control.Applicative

matchHead :: Num a => Op -> String -> Maybe a
matchHead = matchHead' 0
  where
    matchHead' n Finite _ = Just n
    matchHead' n (Compare x rx) (_v1:xs)
      | (x == _v1) = matchHead' (n + 1) rx xs
    matchHead' _ (Compare _ _) _ = Nothing
    matchHead' n (Split p q) xs = (matchHead' n p xs) <|> (matchHead' n q xs)

match :: Num a => Op -> String -> Maybe (a, a)
match = match' 0
  where
    match' n rx xs = (((,) n) <$> (matchHead rx xs)) <|> ((match' (n + 1) rx) =<< (safeTail xs))
