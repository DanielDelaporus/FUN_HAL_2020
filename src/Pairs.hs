module Pairs where

import Parser
import Exec (removeParr)


make_pair :: String -> String
make_pair s = case (parseAnd (parseSome (parseAnyChar ['0'..'9'])) (parseSome (parseAnyChar ". ")) s) of
    Just (x, y) -> case parseChar '(' y of
        Just (x', y') -> (fst x) ++ " " ++ (make_pair (removeParr ("(" ++ y')))
        Nothing -> (fst x) ++ " " ++ (y)
    Nothing -> ""

from_pairs :: String -> String
from_pairs "" = ""
from_pairs s = "(" ++ make_pair (removeParr s) ++ ")"
