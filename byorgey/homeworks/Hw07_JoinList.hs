{-# OPTIONS_GHC -Wall #-}
module Hw07_JoinList where

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)
           
jlist :: JoinList (Product Int) Char
jlist = Append (Product 210)
        (Append (Product 30)
         (Single (Product 5) 'y')
         (Append (Product 6)
          (Single (Product 2) 'e')
          (Single (Product 3) 'a')))
        (Single (Product 7) 'h')

jlbToList :: JoinList m a -> [a]
jlbToList Empty = []
jlbToList (Single _ a) = [a]
jlbToList (Append _ l1 l2) = jlbToList l1 ++ jlbToList l2                       

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty  jls = jls
(+++) jls Empty = jls
(+++) js1 js2  = Append (tag js1 `mappend` tag js2) js1 js2 

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


