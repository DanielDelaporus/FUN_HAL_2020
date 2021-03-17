module Parser where

import Data.Maybe (maybe, fromJust, isNothing)

type  Parser a = String  -> Maybe (a, String)


parseChar :: Char -> Parser Char
parseChar a [] = Nothing
parseChar a (c:s) | a == c = Just (a, s)
                  | otherwise = Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] s = Nothing
parseAnyChar a [] = Nothing
parseAnyChar (c:s) b | parseChar c b /= Nothing =  parseChar c b
                     | otherwise = parseAnyChar s b

parseOr :: Parser a -> Parser a -> Parser a
parseOr f g s = case (f s) of
              Nothing -> g s
              otherwise -> f s

parseAnd  ::  Parser a -> Parser b -> Parser (a,b)
parseAnd f g s = case (f s) of
                Nothing -> Nothing
                Just x -> case (g (snd x)) of
                    Nothing -> Nothing
                    Just y -> Just ((fst x, fst y), snd y)

parseAndWith  :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith m f g s = case (f s) of
                Nothing -> Nothing
                Just x -> case (g (snd x)) of
                    Nothing -> Nothing
                    Just y -> Just (m (fst x) (fst y), snd y)

parseMany :: Parser a -> Parser [a]
parseMany p s
    | isNothing res_p = Just ([], s)
    | otherwise = Just (list, str)
    where res_p = p s
          recursion = parseMany p (tail s)
          list = (fst . fromJust $ res_p) : (fst . fromJust $ recursion)
          str = snd $ fromJust recursion

parseSome :: Parser a -> Parser [a]
parseSome p s
    | isNothing res_and = Nothing
    | otherwise = Just (list, str)
    where res_and = parseAnd (p) (parseMany p) s
          tuple = fst . fromJust $ res_and 
          list = (fst tuple) : (snd tuple) 
          str = snd . fromJust $ res_and

parseUInt :: Parser Int
parseUInt s
    | isNothing res_p = Nothing
    | otherwise = Just (n, str)
    where res_p = parseSome (parseAnyChar $ ['0'..'9']) s
          n = read . fst . fromJust $ res_p :: Int
          str = snd . fromJust $ res_p

parseInt :: Parser Int
parseInt s
    | isNothing $ parseChar '-' s = parseUInt s
    | isNothing res_UI = Nothing
    | otherwise = Just (-(fst $ fromJust res_UI), snd $ fromJust res_UI)
    where res_UI = parseUInt (tail s)
