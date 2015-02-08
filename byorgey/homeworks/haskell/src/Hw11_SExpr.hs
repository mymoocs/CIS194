{- CIS 194 HW 11
   due Monday, 8 April
-}

module Hw11_SExpr where

import Hw11_AParser
import Control.Applicative
import Data.Char(isUpper, isSpace, isAlpha, isAlphaNum)
------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
-- runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")
-- runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" == Just ("","abcdeFGh")
-- zeroOrMore :: Parser a -> Parser [a]


zeroOrMore p = oneOrMore p <|> pure []
               
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

nullP :: Parser [a]
nullP = pure []

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------
-- runParser spaces "   hello"   == Just ("   ","hello")
spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

-- runParser ident "foobar baz" == Just ("foobar"," baz")
-- runParser ident "foo33fA" == Just ("foo33fA","")
-- runParser ident "2bad" == Nothing
-- runParser ident "" == Nothing

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum) 

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
