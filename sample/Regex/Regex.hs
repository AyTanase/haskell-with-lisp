module Regex (module Regex, NFA, module Parse, module Exec) where
import Common
import Parse
import Exec

regex :: Num a => String -> String -> Maybe (a, a)
regex = match . parse
