{--
  Created       : 2016 Nov 22 (Tue) 06:11:56 AM by Arthur Vardanyan.
  Last Modified : 2016 Nov 23 (Wed) 10:40:03 AM by Arthur Vardanyan.
--}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- "orphan instance" - a type class instance C T which is defined in a
-- module which is distinct from both the modules where C and T are defined.


module Cis194.Hw08.Party where

import Cis194.Hw08.Employee

import Data.List (union)
import Data.Tree
-------------------------------------------------------------------------------
-- Exercise 1

-- 1)
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f1) (GL es f) = GL (e:es) (f1 + f)

-- 2)
instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL lgl _) (GL rgl _) =
        let u = lgl `union` rgl
            f = foldr ((+) . empFun) 0 u
        in GL u f

-- 3)
moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2)
    | f1 > f2 = g1
    | otherwise = g2

-------------------------------------------------------------------------------
-- Exercise 2

treeFold :: (a -> b -> b ) -> (b -> b -> b) -> b  -> Tree a -> b
treeFold  f g z (Node a xs) = f a (treeFold' f g z xs)
treeFold' :: (a -> b -> b ) -> (b -> b -> b) -> b -> [Tree a] -> b
treeFold' f g z (t:ts) = g (treeFold f g z t) (treeFold' f g z ts)
treeFold' _ _ z [] = z


-------------------------------------------------------------------------------
-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)]
          -> (GuestList, GuestList)
nextLevel = undefined


-------------------------------------------------------------------------------
-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = undefined

-------------------------------------------------------------------------------
-- Exercise 5






-- End of file
