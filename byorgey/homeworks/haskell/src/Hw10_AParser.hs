{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- 1.

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  -- fmap f (Parser p) = Parser (\s -> p s >>= \(r,rest) -> return (f r, rest))
  fmap f (Parser {runParser = p}) = Parser $  fmap (first f) . p

-----------------
-- Excersize 2 --
-----------------  
  

instance Applicative Parser where
  pure a = Parser (\s -> Just(a, s))
  --  Parser l <*> Parser r = Parser (\s -> l s >>= \(a,rest) -> r rest >>= \(a',rest') -> return (a a', rest'))
  --  Parser l <*>        r = Parser (\s -> l s >>= \(a,rest) -> runParser (a <$> r) rest)
  Parser p1 <*> Parser p2 = Parser f
    where
      f s = case (p1 s) of
        Nothing      -> Nothing
        Just (g, rs) -> case (p2 rs) of
                         Nothing       -> Nothing
                         Just (b, rss) -> Just (g b, rss)
  
type Name = String
data Employee = Emp { name :: Name, phone :: String }

parseName :: Parser Name
parseName = undefined -- TODO

parsePhone :: Parser String
parsePhone = undefined --TODO

parseEmployee :: Parser Employee
parseEmployee = Emp <$> parseName <*> parsePhone


-- 3.

-- 3.1 getexpect to see  'a' and 'b', returns pair ('a','b')
-- runParser abParser "abcdef" == Just (('a','b'),"cdef")
-- runParser abParser "aebcdf" == Nohhing
abParser :: Parser (Char, Char)
abParser = (,) <$> satisfy (=='a') <*> satisfy (=='b')

abParser' = Parser (\s -> case runParser (satisfy (=='a')) s of
                          Nothing -> Nothing
                          Just (a, rs) -> case runParser (satisfy (=='b')) rs of
                                           Nothing -> Nothing
                                           Just (b, rs1) -> Just ((a,b), rs1))

-- 3.2 like 3.1 return () not pair
-- runParser abParser_ "abcdef" == Just ((),"cdef")
-- runParser abParser_ "aebcdf" == Nothing

abParser_ :: Parser ()
abParser_ =
  -- (\a->()) <$> abParser
  -- (\(_,_) -> ()) <$> abParser
  const () <$> abParser

-- 3.2 
-- runParser intPair "12 34" == Just ([12,34],"")

--intPair :: Parser [Int]
intPair = -- (\a b -> [a,b]) <$> posInt <* char ' ' <*> posInt
          -- (\x _ y -> [x,y]) <$> posInt <*> satisfy (' '==) <*> posInt
  Parser (\s -> case runParser posInt s of
           Nothing      -> Nothing
           Just (x, s1) -> case runParser (satisfy (==' ')) s1 of
                            Nothing     -> Nothing
                            Just (c,s2) -> case runParser posInt s2 of
                                            Nothing      -> Nothing
                                            Just (y, s3) -> Just ([x,y], s3))   


-- 4.

{- class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a -}



instance Alternative Parser where
  empty = Parser (const  Nothing)
  p1  <|> p2 = Parser (\s -> case runParser p1 s of
                              Nothing    -> runParser p2 s
                              result -> result)

-- 5.

-- runParser intOrUppercase "342abcd" == Just ((), "abcd")
--  runParser intOrUppercase "XYZ" == Just ((), "YZ")
-- runParser intOrUppercase "foo" == Nothing
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
                 -- (\_ -> ()) <$> posInt <|> (\_-> ()) <$> satisfy isUpper
                 
{-  Parser (\s -> case  runParser ((satisfy isUpper) <|> (satisfy posInt)) s of
                 Nothing      -> Nothing
                 Just (x, s1) -> intOrUppercase s1
                   )-}

