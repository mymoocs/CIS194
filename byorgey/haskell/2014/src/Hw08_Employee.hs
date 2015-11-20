module Hw08_Employee where

import           Data.Tree
import           Data.Monoid

-- Employee names are represented by Strings.
type Name = String

-- The amount of fun an employee would have at the party, represented
-- by an Integer
type Fun  = Integer

-- An Employee consists of a name and a fun score.
data Employee = Emp { empName :: Name, empFun :: Fun }
  deriving (Show, Read, Eq)

-- A small company hierarchy to use for testing purposes.
testCompany :: Tree Employee
testCompany
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 2)
      [ Node (Emp "Joe" 5)
        [ Node (Emp "John" 1) []
        , Node (Emp "Sue" 5) []
        ]
      , Node (Emp "Fred" 3) []
      ]
    , Node (Emp "Sarah" 17)
      [ Node (Emp "Sam" 4) []
      ]
    ]

testCompany2 :: Tree Employee
testCompany2
  = Node (Emp "Stan" 9)
    [ Node (Emp "Bob" 3) -- (8, 8)
      [ Node (Emp "Joe" 5) -- (5, 6)
        [ Node (Emp "John" 1) [] -- (1, 0)
        , Node (Emp "Sue" 5) [] -- (5, 0)
        ]
      , Node (Emp "Fred" 3) [] -- (3, 0)
      ]
    , Node (Emp "Sarah" 17) -- (17, 4)
      [ Node (Emp "Sam" 4) [] -- (4, 0)
      ]
    ]

-- A type to store a list of guests and their total fun score.
data GuestList = GL [Employee] Fun
  deriving (Show, Eq)

instance Ord GuestList where
  compare (GL _ f1) (GL _ f2) = compare f1 f2

-- 1.

-- 1.1
-- take emp and listm, and add emp to list
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ x ) (GL es s) = GL (e:es) (s + x) 

-- 1.2
instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es1 s1) (GL es2 s2) = GL (es1 ++ es2) (s1 + s2)

-- 1.3
-- teake 2 list, return which one has mote fun  
moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if gl1 > gl2 then gl1 else gl2

-- 2.

{-
data Tree a = Node {
  rootLabel :: a, -- label value
  subForest :: [Tree a] -- zero or more child trees
  }
-}

-- 2.1 
-- define fold for Type Tree

-- treeFold :: a -> (a -> Tree t -> a) -> Tree t -> a
treeFold e f tree@(Node {subForest = xs}) = foldl f (f e tree) xs 

tag (Node {rootLabel = x}) = x

------------------------------------------------------------------------------

-- 3.

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gl =  undefined
