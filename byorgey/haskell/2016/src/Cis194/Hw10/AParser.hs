{- CIS 194 HW 10
   due Monday, 1 April

 Created       : 2016 Nov 26 (Sat) 05:57:45 PM by Arthur Vardanyan.
 Last Modified : 2017 Jan 04 (Wed) 07:05:41 PM by Arthur Vardanyan.
--}

module Cis194.Hw10.AParser where

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


-- |
--
-- >>> runParser (satisfy isUpper) "ABC"
-- Just ('A',"BC")
-- >>> runParser (satisfy isUpper) "abc"
-- Nothing
-- >>> runParser (char 'x') "xyz"
-- Just ('x',"yz")

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

-------------------------------------------------------------------------------
-- Exercise 1.
-- | implement a Functor instance for Parser.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
   -- fmap :: (a -> b) -> f a -> f b
    fmap f p = Parser $ \s ->  runParser p s >>= \(rs, rest) -> return (f rs, rest)


-------------------------------------------------------------------------------
-- Exercise 2.

instance Applicative Parser where
    -- <*> :: f (a->b)->f a -> f b
    pure a = Parser (\s -> return (a, s))
    (Parser p1) <*>  (Parser p2) =
        Parser $ \s -> p1 s >>=
                       \(f, rest1) -> p2 rest1 >>=
                                      \(rs, rest2) -> return (f rs, rest2)
    -- Parser l <*> r = Parser (\s -> l s >>= \(a,rest) -> runParser (a <$> r) rest)

-- type Name = String
-- data Employee = Emp { name :: Name, phone :: String }


-- | we could now use the Applicative instance for Parser to make an
-- employee parser from name and phone parsers. That is, if
-- parseName :: Parser Name
-- parsePhone :: Parser String

-- then
-- Emp <$> parseName <*> parsePhone :: Parser Employee

-------------------------------------------------------------------------------
-- Exercise 3.

-- |
--
-- >>> runParser abParser "abcdef"
-- Just ((’a’,’b’),"cdef")
-- >>> runParser abParser "aebcdf"
-- Nothing

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

-- |
--
-- >>> runParser abParser_ "abcdef"
-- Just ((),"cdef")
-- >>> runParser abParser_ "aebcdf"
-- Nothing

abParser_ :: Parser ()
abParser_ = () <$ abParser
    -- (\(_,_) -> ()) <$> abParser

-- |
--
-- >>> runParser intPair "12 34"
-- Just ([12,34],"")

intPair :: Parser [Integer]
intPair = (\x _ y -> [x,y]) <$> posInt <*> satisfy (' '==) <*> posInt


-------------------------------------------------------------------------------
-- Exercise 4.

instance Alternative  Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \s -> case p1 s of
                         Nothing -> p2 s
                         res     -> res

-------------------------------------------------------------------------------
-- Exercise 5.

-- |
--
-- >>> runParser intOrUppercase "342abcd"
-- Just ((), "abcd")
-- >>> runParser intOrUppercase "XYZ"
-- Just ((), "YZ")
-- >>> runParser intOrUppercase "foo"
-- Nothing
intOrUppercase :: Parser ()
intOrUppercase  = () <$ satisfy isUpper <|>  () <$ posInt
