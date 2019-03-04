{-# LANGUAGE FlexibleContexts #-}

module Parse (Parse.parse) where
import Common
import Text.Parsec as P
import Data.Function (fix)


parse :: String -> Op
parse s = case P.parse regex "" s of
  Left e -> error $ show e
  Right r -> r Finite


regex, rConcat :: Stream s m Char => ParsecT s u m NFA

rSplit :: Stream s m Char => NFA -> ParsecT s u m NFA

regex = rConcat >>= rSplit

rConcat = fmap (foldr (.) id) (many rAtom)

rSplit f = (char '|' >> fmap (split f) regex) <|> return f


rAtom, rChar, rEscape, rGroup, rGroup' :: Stream s m Char => ParsecT s u m NFA

rAtom = quantify =<< (rChar <|> rEscape <|> rGroup)

rChar = fmap Compare $ noneOf "\\|()"

rEscape = char '\\' >> fmap Compare anyChar

rGroup = between (char '(') (char ')') rGroup'

rGroup' = (try (string "?>") >> fmap cut regex) <|> regex


quantify :: Stream s m Char => NFA -> ParsecT s u m NFA

quantify f = quantifier '*' makeStar f <|> quantifier '+' makePlus f <|> quantifier '?' makeOpt f <|> return f


type QMaker = (NFA -> NFA -> NFA) -> NFA -> NFA

quantifier :: Stream s m Char => Char -> QMaker -> NFA -> ParsecT s u m NFA

quantifier c make f = char c >> fmap (flip make f) greediness


greediness :: Stream s m Char => ParsecT s u m (NFA -> NFA -> NFA)

greediness = (char '+' >> return atomic) <|> (char '?' >> return (flip split)) <|> return split


makeStar, makePlus, makeOpt :: QMaker

makeStar method f = fix $ \g -> method (f . g) id

makePlus method f = f . makeStar method f

makeOpt method f = method f id
