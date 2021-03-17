module Tree where

import Parser

data Tree = EmptyTree
        | Tree {
            value :: String,
            children :: [Tree]
            }
        | Error
        deriving (Show, Eq)


createTree :: String -> Tree
createTree [] = EmptyTree
createTree file
    | checkNbParr file = createTree' (takeParrContent file)
    | otherwise        = Error

createTree' :: String -> Tree
createTree' pCont
    | isPContError pCont = Error
    | head pCont == '\'' = Tree "'" ([left'] ++ [right'])
    | otherwise = Tree value ([left] ++ [right])
    where value = head $ words pCont
          args = drop (length value + 1) pCont
          (left, right) = separateArgs args
          (left', right') = separateArgs $ tail pCont

separateArgs :: String -> (Tree, Tree)
separateArgs [] = (EmptyTree, EmptyTree)
separateArgs str = case head str of
    '(' -> (createTree str, diffVarParr $ drop (length parr1 + 3) str)
    _   -> (createAtom var1, diffVarParr $ drop (length (head var1) + 1) str)
    where parr1 = takeParrContent str
          var1  = words str
          diffVarParr [] = EmptyTree
          diffVarParr s = case head s of
            '(' -> createTree s
            _   -> createAtom $ words s
          createAtom v = case v of
            [] -> EmptyTree
            _  -> Tree (head v) ([EmptyTree] ++ [EmptyTree])

removeFirst :: String -> String
removeFirst (c:s) = s
removeFirst [] = ""

isPContError :: String -> Bool
isPContError str
    | length str < 5 = False
    | otherwise = (take 5 str) == "ERROR" || isPContError (tail str)

takeParseContent :: String -> String
takeParseContent [] = "ERROR"
takeParseContent (c:s) = case c of
    '(' -> parrContent 0 s
    _   -> "ERROR"
    where parrContent 0 (')':_) = []
          parrContent _ [] = "ERROR"
          parrContent n s = case ((parseChar '(' s), (parseChar ')' s)) of
            (Just (a, b), _) -> a : parrContent (n + 1) b
            (_, Just (a, b)) -> a : parrContent (n - 1) b
            (Nothing, Nothing) -> c : parrContent n (removeFirst s)

checkNbParr :: String -> Bool
checkNbParr file = checkNbParr' 0 file
    where checkNbParr' n [] = case n of
            0   -> True
            _   -> False
          checkNbParr' n (c:f) = case c of
            '(' -> checkNbParr' (n + 1) f
            ')' -> checkNbParr' (n - 1) f
            _   -> checkNbParr' n       f

readTree :: Tree -> String
readTree EmptyTree = []
readTree (Tree v (EmptyTree:s)) = v
readTree (Tree v children) = "(" ++ v ++ " " ++ (readTree (children !! 0)) ++ spc ++ right ++ ")"
    where right = readTree (children !! 1)
          spc = case right of
            [] -> []
            _  -> " "

takeParrContent :: String -> String
takeParrContent [] = "ERROR"
takeParrContent (c:s) = case c of
    '(' -> parrContent 0 s
    _   -> "ERROR"
    where parrContent 0 (')':_) = []
          parrContent _ [] = "ERROR"
          parrContent n (c:s) = case c of
            '(' -> c : parrContent (n + 1) s
            ')' -> c : parrContent (n - 1) s
            _   -> c : parrContent n       s
