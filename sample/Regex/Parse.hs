{-# LANGUAGE FlexibleContexts #-}

module Parse (Parse.parse) where
import Common
import Control.Monad
import Text.Parsec as P
import Data.Function (fix)


parse :: String -> Op
parse s = case P.parse regex "" s of
  Left e -> error $ show e
  Right r -> r Finite


regex, rConcat :: Stream s m Char => ParsecT s u m NFA

rSplit :: Stream s m Char => NFA -> ParsecT s u m NFA

regex = rConcat >>= rSplit

rConcat = liftM (foldr (.) id) (many rAtom)

rSplit f = (char '|' >> liftM (split f) regex) <|> return f


rAtom, rChar, rEscape, rGroup, rGroup' :: Stream s m Char => ParsecT s u m NFA

rAtom = quantify =<< (rChar <|> rEscape <|> rGroup)

rChar = liftM Compare $ noneOf "\\|()"

rEscape = char '\\' >> liftM Compare anyChar

rGroup = between (char '(') (char ')') rGroup'

rGroup' = (try (string "?>") >> liftM cut regex) <|> regex


quantify :: Stream s m Char => NFA -> ParsecT s u m NFA

quantify f = quantifier '*' makeStar f <|> quantifier '+' makePlus f <|> quantifier '?' makeOpt f <|> return f


type QMaker = (NFA -> NFA -> NFA) -> NFA -> NFA

quantifier :: Stream s m Char => Char -> QMaker -> NFA -> ParsecT s u m NFA

quantifier c make f = char c >> ((char '+' >> return (make atomic f)) <|> (char '?' >> return (make (flip split) f)) <|> return (make split f))


makeStar, makePlus, makeOpt :: QMaker

makeStar method f = fix $ \g -> method (f . g) id

makePlus method f = f . makeStar method f

makeOpt method f = method f id
