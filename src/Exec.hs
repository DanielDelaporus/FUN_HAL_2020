module Exec where

import Parser
import Read
import Tree

import System.Environment
import System.Exit

data Var = Var
    { name :: String,
      val :: String
    } deriving (Show)


getTriple :: [String] -> (String, String, String)
getTriple (a:b:c:[]) = (a, b, c)

invocVar :: String -> Var
invocVar s = case getTriple (words (removeParr s)) of
    ("define",b,c) -> Var { name = b,
                            val = c}
    _ ->  Var { name = "ERROR", val = "ERROR"}

invocAllVar :: String -> [Var]
invocAllVar s = map invocVar (removeEmptyLine (lines s))

removeParr :: String -> String
removeParr ('(':s) = case (parseChar ')' (reverse s)) of
    Just (x, y) -> reverse y 
    Nothing -> ""
removeParr s = s

checkVar :: String -> [Var] -> Int
checkVar s ((Var name value):db) = case (s == name) of
                True -> read value :: Int
                False -> checkVar s db
checkVar s [] = read s :: Int


executeCond :: Tree -> [Var] -> (String, [Var])
executeCond (Tree "cond" children) db = case (children !! 0) of
            (Tree "#t" child) -> (fst (executeTree (child !! 0) db), db)
            (Tree "#f" child) -> case (children !! 1) of
                (Tree "#t" lchild) -> (fst (executeTree (lchild !! 0) db), db) 
                (Tree "#f" lchild) -> ("", db)
                (Tree v lchild) -> case fst (executeTree (createTree v) db) of
                    "#t" -> (fst (executeTree (lchild !! 0) db), db)
                    "#f" -> ("", db)
                    _ -> ("something is wrong", db)
            (Tree v child) -> case fst (executeTree (createTree v) db) of
                "#t" -> (fst (executeTree (child !! 0) db), db)
                "#f" -> case (children !! 1) of
                    (Tree "#t" lchild) -> (fst (executeTree (lchild !! 0) db), db) 
                    (Tree "#f" lchild) -> ("", db)
                    (Tree v lchild) -> case fst (executeTree (createTree v) db) of
                        "#t" -> (fst (executeTree (lchild !! 0) db), db)
                        "#f" -> ("", db)
                        _ -> ("something is wrong", db)
                _ -> ("something is wrong", db)


executeTreeMaths :: Tree -> [Var] -> (Int, [Var])
executeTreeMaths (Tree "*" children) db =   ((fst (executeTreeMaths (children !! 0) db))   *   (fst (executeTreeMaths (children !! 1) db)), db)
executeTreeMaths (Tree "div" children) db = ((fst (executeTreeMaths (children !! 0) db)) `div` (fst (executeTreeMaths (children !! 1) db)), db)
executeTreeMaths (Tree "mod" children) db = ((fst (executeTreeMaths (children !! 0) db)) `mod` (fst (executeTreeMaths (children !! 1) db)), db)
executeTreeMaths (Tree "+" children) db =   ((fst (executeTreeMaths (children !! 0) db))   +   (fst (executeTreeMaths (children !! 1) db)), db)
executeTreeMaths (Tree "-" children) db =   ((fst (executeTreeMaths (children !! 0) db))   -   (fst (executeTreeMaths (children !! 1) db)), db)
executeTreeMaths (Tree s children) db = (checkVar s db, db)


executeTree :: Tree -> [Var] -> (String, [Var])
executeTree (Tree "atom?" children) db = case (children !! 0) of
    EmptyTree -> ("#f", db)
    _         -> isAtomic (children !! 0)
    where isAtomic (Tree _ child) | (child !! 0) == EmptyTree && (child !! 1) == EmptyTree = ("#t", db)
                                  | otherwise = ("#f", db)
executeTree (Tree "*" children) db =   (show (fst (executeTreeMaths (Tree "*" children)   db)), db)
executeTree (Tree "div" children) db = (show (fst (executeTreeMaths (Tree "div" children) db)), db)
executeTree (Tree "mod" children) db = (show (fst (executeTreeMaths (Tree "mod" children) db)), db)
executeTree (Tree "+" children) db =   (show (fst (executeTreeMaths (Tree "+" children)   db)), db)
executeTree (Tree "-" children) db =   (show (fst (executeTreeMaths (Tree "-" children)   db)), db)
executeTree (Tree "<" children) db = case ((read (fst (executeTree (children !! 0) db)) :: Int) < (read (fst (executeTree (children !! 1) db)) :: Int)) of
            True -> ("#t", db)
            False -> ("#f", db)
executeTree (Tree "eq?" children) db = case ((read (fst (executeTree (children !! 0) db)) :: Int) == (read (fst (executeTree (children !! 1) db)) :: Int)) of
            True -> ("#t", db)
            False -> ("#f", db)
executeTree (Tree "define" ((Tree vl child):right:s)) db = ("" , db ++ [Var {name = vl, val = fst (executeTree right db)}])
executeTree (Tree "cons" (EmptyTree:s)) db = ("()", db)
executeTree (Tree "cons" children) db = ("(" ++ fst (executeTree (children !! 0) db) ++ " . " ++ fst (executeTree (children !! 1) db) ++ ")", db)
executeTree (Tree "car" ((Tree "cons" child):s)) db = (fst (executeTree (child !! 0) db), db)
executeTree (Tree "cdr" ((Tree "cons" child):s)) db = (fst (executeTree (child !! 1) db), db)
executeTree (Tree "quote" children) db = (readTree (children !! 0), db)
executeTree (Tree "'" children) db = (readTree (children !! 0), db)
executeTree (Tree "cond" children) db = executeCond (Tree "cond" children) db
executeTree (Tree s children) db = (show (checkVar s db), db)


executeFile :: [String] -> [Var] -> IO()
executeFile (c:s) db = case executeTree (createTree c) db of
                    ("", b) -> putStr "" >>
                               executeFile s b
                    ("something is wrong", b) -> exitWith (ExitFailure 84)
                    (a, b) -> putStrLn a >>
                              executeFile s b
executeFile [] db = putStr ""

executeAllFile :: String -> [Var] -> IO ()
executeAllFile s = executeFile (removeEmptyLine (lines s))
