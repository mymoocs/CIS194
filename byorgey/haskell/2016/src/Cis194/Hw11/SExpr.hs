{- CIS 194 HW 11

  Created       : 2017 Jan 04 (Wed) 08:41:14 PM by Arthur Vardanyan.
  Last Modified : 2017 Jan 07 (Sat) 07:10:49 PM by Arthur Vardanyan.

-}
-- http://comonad.com/reader/2012/abstracting-with-applicatives/
module Cis194.Hw11.SExpr where

import Cis194.Hw10.AParser

import Control.Applicative

import Data.Char (isSpace, isAlpha, isAlphaNum,  isNumber)
-- lecture examples
data Emp = Emp { name :: String, phone :: String } deriving (Eq, Show)

names :: [String]
names  = ["Joe", "Sara", "Mae"]

phones :: [String]
phones = ["555-5555", "123-456-7890", "555-4321"]

employees1 :: [Emp]
employees1 = Emp <$> names <*> phones

-- | nondeterministic arithmetic
--
-- >>> (Emp <$> ["A", "B"] <*> ["1", "2"]
-- [Emp "A" "1",Emp "A" "2",Emp "B" "1",Emp "B" "2"]
-- >>> ([4,5] .* pure 2) .+ [6,1]  -- (either 4 or 5) times 2, plus either 6 or 1
-- [14,9,16,11]
-- >>> (Just 3 .+ Just 5) .* Just 8
-- Just 64
-- >>> (Just 3 .+ Nothing) .* Just 8
-- Nothing
-- >>> [1..2] .* [1..4]
-- [1,2,3,4,2,4,6,8]
-- >>> employees1
-- [Emp {name = "Joe", phone = "555-5555"},Emp {name = "Joe", phone = "123-456-7890"},Emp {name = "Joe", phone = "555-4321"},Emp {name = "Sara", phone = "555-5555"},Emp {name = "Sara", phone = "123-456-7890"},Emp {name = "Sara", phone = "555-4321"},Emp {name = "Mae", phone = "555-5555"},Emp {name = "Mae", phone = "123-456-7890"},Emp {name = "Mae", phone = "555-4321"}]
(.+), (.*) :: Applicative f => f Integer  ->  f Integer  -> f Integer
(.+) = liftA2 (+)    -- addition lifted to some Applicative context
(.*) = liftA2 (*)    -- same for multiplication

-- | To parse something but ignore its output,
--
-- >>> runParser (spaces *> posInt) " 345"
-- Just (345,"")

(.<*) :: Applicative f => f a -> f b -> f a
(.<*) fa _ = fa

(.*>) :: Applicative f => f a -> f b -> f b
(.*>) _ fb = fb


------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- |
--
-- >>> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- >>> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
-- Just ("ABC","dEfgH")
-- >>> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
-- Just ("","abcdeFGh")
-- >>> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
-- Nothing

-- To parse one or more occurrences of p, run p once and then parse zero or
-- more occurrences of p. To parse zero or more occurrences of p, try parsing one
-- or more; if that fails, return the empty list.

-- | Zero or more.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []


-- | One or more.
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
-- | parse spaces
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- https://stackoverflow.com/questions/9013310/can-someone-explain-where-applicative-instances-arise-in-this-code
alphaNum :: Char -> Bool
alphaNum = (||) <$> isAlpha <*> isNumber

-- | identifier can be any nonempty sequence of letters and digits, except
-- that it may not start with a digit.
--
-- >>> runParser ident "foobar baz"
-- Just ("foobar"," baz")
-- >>> runParser ident "foo33fA"
-- Just ("foo33fA","")
-- >>> runParser ident "2bad"
-- Nothing
-- >>> runParser ident ""
-- Nothing

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*>
        zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving (Eq,Show)

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Eq,Show)



-- valid S-expressions:
{--
5
foo3
(bar (foo) 3 5 874)
(((lambda x (lambda y (plus x y))) 3) 5)
( lots of ( spaces in ) this ( one ) )
--}

-- atom ::= int | ident
atom :: Parser SExpr
atom = A <$> ((N <$> posInt) <|> (I  <$> ident))

atom' :: Parser SExpr
atom' = (\x -> A x)  <$> (((\n -> N n) <$> posInt) <|> ((\i -> I i) <$> ident))

-- S ::= atom | (S∗*)
parseSExpr :: Parser SExpr
parseSExpr = spaces *>
             (atom
              <|>
              (satisfy (== '(') *> (Comb <$> oneOrMore parseSExpr) <* satisfy (==')')))
             <* spaces
