module Exec where
import Common
import Control.Applicative
matchHead :: Num a => (NFA -> [Char] -> (Maybe a))
matchHead = (matchHead' 0) where { matchHead' n Finite _ = (Just n); matchHead' _ (Compare _ _) [] = Nothing; matchHead' n (Compare x rx) (y:ys) = (if ((==) x y) then (matchHead' (((+) 1) n) rx ys) else Nothing); matchHead' n (Split p q) xs = ((<|>) (matchHead' n p xs) (matchHead' n q xs)); }
match :: Num a => (NFA -> [Char] -> (Maybe (a, a)))
match = (match' 0) where { match'' n rx xs = (fmap ((,) n) (matchHead rx xs)); match' n rx [] = (match'' n rx []); match' n rx xs = ((<|>) (match'' n rx xs) (match' (((+) 1) n) rx (tail xs))); }
