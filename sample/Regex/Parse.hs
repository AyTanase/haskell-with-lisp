module Parse (parse) where
import Common
import Data.Bifunctor
type QMaker a = ((BinOp (End a)) -> (End (End a)))
makeStar, makePlus, makeOpt :: (QMaker a)
makeStar method f = let
  g = (method ((.) f g) id)
  in g
makePlus method f = ((.) f (makeStar method f))
makeOpt method = (flip method id)
type Parser a b = ((End b) -> [a] -> ((End b), [a]))
parse' :: (Parser Char NFA)
checkQ'' :: ((QMaker NFA) -> (Parser Char NFA))
checkQ'' make f ('?':xs) = ((make (flip (destribute Split)) f), xs)
checkQ'' make f xs = ((make (destribute Split) f), xs)
checkQ' :: (Parser Char NFA)
checkQ' f ('*':xs) = (checkQ'' makeStar f xs)
checkQ' f ('+':xs) = (checkQ'' makePlus f xs)
checkQ' f ('?':xs) = (checkQ'' makeOpt f xs)
checkQ' f xs = (f, xs)
checkQ :: ((End NFA) -> (Parser Char NFA))
checkQ f g xs = let
  (h, ys) = (checkQ' g xs)
  in (parse' ((.) f h) ys)
parseChar :: ((End NFA) -> Char -> [Char] -> ((End NFA), [Char]))
parseChar f x xs = (checkQ f (Compare x) xs)
parse' f [] = (f, [])
parse' f ('\\':x:xs) = (parseChar f x xs)
parse' f ('(':xs) = let
  (g, ys) = (parse' id xs)
  in (checkQ f g ys)
parse' f (')':xs) = (f, xs)
parse' f ('|':xs) = (first (destribute Split f) (parse' id xs))
parse' f (x:xs) = (parseChar f x xs)
parse :: ([Char] -> NFA)
parse xs = (fst (parse' id xs) Finite)
