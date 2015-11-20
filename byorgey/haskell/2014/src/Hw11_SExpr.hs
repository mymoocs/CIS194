{- CIS 194 HW 11
   due Monday, 8 April
-}

{-# OPTIONS_GHC -Wall #-}

module Hw11_SExpr where

import Hw11_AParser
import Control.Applicative
import Data.Char(isUpper, isSpace, isAlpha, isAlphaNum)

import qualified Test.HUnit          as T
import qualified Test.HUnit.Util     as U


------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
-- runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")
-- runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" == Just ("","abcdeFGh")
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []
               
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

nullP :: Parser [a]
nullP = pure []

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "e10" (runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH") (Just ("ABC","dEfgH"))
    , U.teq "e11" (runParser (oneOrMore  (satisfy isUpper)) "ABCdEfgH") (Just ("ABC","dEfgH"))
    , U.teq "e12" (runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh") (Just ("","abcdeFGh"))
    , U.teq "e13" (runParser (oneOrMore  (satisfy isUpper)) "abcdeFGh") Nothing
    ]

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
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum) 

ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "e20" (runParser ident  "foobar baz") (Just ("foobar"," baz"))
    , U.teq "e21" (runParser ident  "foo33fA")    (Just ("foo33fA",""))
    , U.teq "e22" (runParser ident  "2bad")       Nothing
    , U.teq "e23" (runParser ident  "")           Nothing
    , U.teq "e23" (runParser spaces "   hello")   (Just ("   ","hello"))
    ]


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
  deriving (Eq, Show)

parseSExpr :: Parser SExpr
parseSExpr = comb

atom :: Parser SExpr
atom =  A <$> (N <$> posInt <|> I <$> ident)

comb :: Parser SExpr
comb = spaces *> (atom <|>
                  satisfy (=='(') *> ( Comb <$> oneOrMore  comb) <* (satisfy (==')')))
       <* spaces

ex3 :: T.Test
ex3 = T.TestList
    [
      U.teq "e30" (runParser parseSExpr "5")
                  (Just (A (N 5),""))
    , U.teq "e31" (runParser parseSExpr "foo3")
                  (Just (A (I "foo3"),""))
    , U.teq "e32" (runParser parseSExpr "foo bar")
                  (Just (A (I "foo"),"bar"))
    , U.teq "e33" (runParser parseSExpr "(foo)")
                  (Just (Comb [A (I "foo")],""))
    , U.teq "e34" (runParser parseSExpr "(foo bar)")
                  (Just (Comb [A (I "foo"),A (I "bar")],""))
    , U.teq "e35" (runParser parseSExpr "(bar (foo) 3 5 874)")
                  (Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],""))
    , U.teq "e36" (runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)")
                  (Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],""))
    , U.teq "e37" (runParser parseSExpr "( lots of ( spaces in ) this ( one ) )")
                  (Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],""))
    ]


hw11 :: IO T.Counts
hw11 = do
  _ <- T.runTestTT ex1
  _ <- T.runTestTT ex2
  T.runTestTT ex3

  
