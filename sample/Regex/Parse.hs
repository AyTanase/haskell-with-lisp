module Parse (parse) where
import Common
import Data.Function (fix)
import Data.Bifunctor (first)


{- Q :: Quantifier -}
type QMaker = (NFA -> NFA -> NFA) -> NFA -> NFA

makeStar, makePlus, makeOpt :: QMaker

makeStar method f = fix $ \g -> method (f . g) id

makePlus method f = f . makeStar method f

makeOpt method f = method f id


type Parser = NFA -> String -> (NFA, String)
parse' :: Parser


{- G :: Greediness -}
checkG :: QMaker -> Parser

checkG make f ('+':xs) = (make atomic f, xs)

checkG make f ('?':xs) = (make (flip split) f, xs)

checkG make f xs = (make split f, xs)


checkQ' :: Parser
checkQ' f ('*':xs) = checkG makeStar f xs
checkQ' f ('+':xs) = checkG makePlus f xs
checkQ' f ('?':xs) = checkG makeOpt f xs
checkQ' f xs = (f, xs)

checkQ :: NFA -> Parser
checkQ f g xs = let
  (h, ys) = checkQ' g xs
  in parse' (f . h) ys

parseChar :: NFA -> Char -> String -> (NFA, String)
parseChar f x xs = checkQ f (Compare x) xs


parseGroup :: String -> (NFA, String)

parseGroup ('?':'>':xs) = first cut $ parse' id xs

parseGroup xs = parse' id xs


parse' f [] = (f, [])

parse' f ('\\':x:xs) = parseChar f x xs

parse' f ('(':xs) = uncurry (checkQ f) (parseGroup xs)

parse' f (')':xs) = (f, xs)

parse' f ('|':xs) = first (split f) (parse' id xs)

parse' f (x:xs) = parseChar f x xs


parse :: String -> Op
parse xs = fst (parse' id xs) Finite
