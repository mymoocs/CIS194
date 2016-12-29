module Cis194.Hw07.JoinList where

import Data.Monoid
import Cis194.Hw07.Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Exercise 1
jlist :: JoinList (Product Int) Char
jlist = Append (Product 210)
        (Append (Product 30)
                    (Single (Product 5)'y')
                    (Append (Product 6)
                                (Single (Product 2) 'e')
                                (Single (Product 3) 'a')
                    )
        )
        (Single (Product 7) 'h')
foldjoinlist :: (t1 -> t -> t2) -> (t1 -> t2 -> t2 -> t2) -> t2 -> JoinList t1 t -> t2
foldjoinlist _ _ z Empty = z
foldjoinlist f _ _ (Single m a) = f m a
foldjoinlist f g z (Append m left right) =
               g m (foldjoinlist f g z left) (foldjoinlist f g z right)


-- foldjoinlist (\m _ -> m) (\a b c -> a <> b <> c) mempty jlist
-- foldjoinlist (\_ -> (:"")) (\_ -> (++)) [] jlist


-- | append two JoinLists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) xs ys = Append (tag xs <> tag ys) xs ys


-- | gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag (Append m _ _) = m
tag (Single m _) = m
tag Empty = mempty


(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

-- | converting join-lists into lists
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlToList' :: JoinList m a -> [a]
jlToList' = foldjoinlist (\_ a-> [a]) (\_ -> (++)) []
-------------------------------------------------------------------------------
-- Exercise 2

-- | fast indexing into a JoinList

-- TODO: fix type signature error, to reproduce uncomment indexJ type
-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _  v) = Just v
indexJ n (Append _ (Single _ v0) (Single _ v1)) = Just $  if n == 0 then v0 else v1
indexJ i (Append _ left right) | i < s1 = indexJ i left
                               | otherwise = indexJ (i-s1) right
                               where
                                 s1 :: Int
                                 s1 = getSize $ tag left

{--Append (Size 5)
           (Append (Size 4)
                       (Append (Size 3)
                                   (Append (Size 2)
                                               (Single (Size 1) "a")
                                               (Single (Size 1) "b"))
                                   (Single (Size 1) "c"))
                       (Single (Size 1) "d"))
           (Single (Size 1) "e")
--}
{-- Append (Size 4)
           (Append (Size 2)
                       (Single (Size 1) 'y')
                       (Single (Size 1) 'e'))
           (Append (Size 2)
                       (Single (Size 1) 'a')
                       (Single (Size 1) 'h'))
--}


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = undefined

dropJ :: (Eq a, Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = undefined
