module Parse (parse) where
import Common
import Data.Function
import Data.Bifunctor


{- Q :: Quantifier -}
type QMaker = BinOp Node -> Node -> Node

makeStar, makePlus, makeOpt :: QMaker

makeStar method f = fix (\g -> method (f . g) id)

makePlus method f = f . (makeStar method f)

makeOpt method f = method f id


type Parser = Node -> String -> (Node, String)
parse' :: Parser


{- G :: Greediness -}
checkG :: QMaker -> Parser

checkG make f ('?':xs) = (make (flip split) f, xs)

checkG make f xs = (make split f, xs)


checkQ' :: Parser
checkQ' f ('*':xs) = checkG makeStar f xs
checkQ' f ('+':xs) = checkG makePlus f xs
checkQ' f ('?':xs) = checkG makeOpt f xs
checkQ' f xs = (f, xs)

checkQ :: Node -> Parser
checkQ f g xs = let
  (h, ys) = checkQ' g xs
  in parse' (f . h) ys

parseChar :: Node -> Char -> String -> (Node, String)
parseChar f x xs = checkQ f (Compare x) xs


parse' f [] = (f, [])

parse' f ('\\':x:xs) = parseChar f x xs

parse' f ('(':xs) = uncurry (checkQ f) (parse' id xs)

parse' f (')':xs) = (f, xs)

parse' f ('|':xs) = first (split f) (parse' id xs)

parse' f (x:xs) = parseChar f x xs


parse :: String -> NFA
parse xs = fst (parse' id xs) Finite
