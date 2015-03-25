{-# OPTIONS_GHC -Wall #-}

module Hw05 where
import Hw05_Ring
import Hw05_Parser


--------------------------------------------------------------------------------
--- Rings
--------------------------------------------------------------------------------


class Ring a where
  addId :: a -- additive identity
  addInv :: a -> a -- additive inverse
  mulId :: a -- multiplicative identity
  add :: a -> a -> a -- addition
  mul :: a -> a -> a -- multiplication










--------------------------------------------------------------------------------
--- Parser
--------------------------------------------------------------------------------
