module Read where

import Data.List


removeEmptyLine :: [String] -> [String]
removeEmptyLine arr = case (hasEmptyLine arr) of
        True -> delete "" arr
        False -> arr

hasEmptyLine :: [String] -> Bool
hasEmptyLine []     = False
hasEmptyLine (x:list) = case (null x) of
        True -> True
        False -> hasEmptyLine list
